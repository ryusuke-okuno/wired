;;;; wired.lisp
;;; The main file of the WIRED project. here are my attempts at
;;; implementing the protocol

(in-package #:wired)

(defparameter *peers-count* 0)
(defparameter *peers* (make-hash-table))

(defconstant +wired-port+ 6666)

(defparameter *output-lock* (bt:make-lock))

(defun locked-format (stream control-string &rest arguments)
  "Use lock to access *standart-output*"
  (bt:with-lock-held (*output-lock*)
	(apply #'format `(,stream ,control-string ,@arguments))))

(defun socket-stream-format (stream control-string &rest args)
  "Force the output after format"
  (apply #'format `(,stream ,control-string ,@args))
  (force-output stream))

(defun wired-ping-host (host)
  "Check if host is up by sending a ping request"
  (handler-case
	  (usocket:with-client-socket (socket stream host +wired-port+ :timeout 5 :element-type 'character)
		(socket-stream-format stream "WIRED/0.1 PING~%")
		(when (usocket:wait-for-input `(,socket) :timeout 30 :ready-only t)
		  (equal (read-line stream) "WIRED/0.1 PONG")))
	(t (c)
	  (declare (ignore c))
	  nil)))

(defclass node ()
  ((server-thread :type bt:thread)
   (master-socket :reader node-server-socket)
   (output :reader node-output)
   (host :initarg :host
		 :initform "0.0.0.0"
		 :reader host
		 :type string)
   (port :initarg :port
		 :initform (error "Must specify port!")
		 :reader port
		 :type fixnum)
   (nodes-inbound :initform nil
				  :reader nodes-inbound)
   (nodes-outbound :initform nil)))

(defclass node-connection ()
  ((thread :type bt:thread
		   :reader node-connection-thread)
   (id :initform (generate-id)
	   :reader node-connection-id
	   :type string)
   (main-node :initarg :main-node
			  :initform (error "Must specify main node!")
			  :type node)
   (socket :initarg :socket
		   :initform (error "Must specify socket!")
		   :reader node-connection-socket)
   (should-stop :initform nil
				:accessor should-stop)))

(define-condition connecting-to-myself () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "You cannot connect to your own server!"))))

(defun socket-timeout-read-line (socket timeout)
  (when (usocket:wait-for-input `(,socket) :timeout timeout :ready-only t)
	(read-line (usocket:socket-stream socket))))

;; (defun start-server (node)
;;   "Start TCP server of the node that can handle multiple clients"
;;   (with-slots (output nodes-inbound port) node
;; 	(labels ((server-log ))
;; 	  (server-log "Starting server...")
;; 	  (let ((master-socket (usocket:socket-listen "0.0.0.0" port
;; 												  :reuse-address t
;; 												  :backlog 256)))
;; 		(unwind-protect
;; 			 (loop (loop :for socket :in (usocket:wait-for-input (cons master-socket nodes-inbound)
;; 																 :ready-only t)
;; 						 :do (if (eq socket master-socket)
;; 								 (let ((new-connection (usocket:socket-accept master-socket :element-type 'character)))
;; 								   (push new-connection nodes-inbound)
;; 								   (server-log "New socket connected!"))
;; 								 (handler-case (process-socket socket)
;; 								   (end-of-file (e)
;; 									 (declare (ignore e))
;; 									 (server-log "Someone left the server")
;; 									 (setf nodes-inbound (delete socket nodes-inbound))
;; 									 (usocket:socket-close socket))))))
;; 		  (server-log "Cleaning up...")
;; 		  (mapc #'usocket:socket-close nodes-inbound)
;; 		  (usocket:socket-close master-socket))))))

(defun connect-to-node (node host port)
  "Connect to the node at host:port. Don't forget to close it!"
  (with-slots (nodes-inbound nodes-outbound) node
	(when (not (and (equal (host node) host)
					(= (port node) port)))
	  (error 'connecting-to-myself))
	(if (some (lambda (n) (and (= (port n) port)
						  (equal (host n) host)))
			  nodes-inbound)
		(format t "Already connected to this node!~%")
		(handler-case (push (usocket:socket-connect host port :timeout 5)
							nodes-outbound)
		  (t (c)
			(declare (ignore c))
			(locked-format t "Failed to connect to node!~%"))))))

(defmethod initialize-instance :after ((instance node) &key)
  (with-slots (server-thread output host port master-socket nodes-inbound)
	  instance
	(setf output *standard-output*
		  master-socket (usocket:socket-listen host port :reuse-address t)
		  server-thread (bt:make-thread
						 (lambda ()
						   (node-log instance "Starting server...")
						   (unwind-protect
								(loop (push (make-instance 'node-connection
														   :main-node instance
														   :socket (usocket:socket-accept master-socket))
											nodes-inbound))
							 (dolist (connection nodes-inbound)
							   (stop-node-connection connection)
							   (node-log instance "Waiting for thread ~a to stop..."
										 (node-connection-id connection))
							   (bt:join-thread (node-connection-thread connection)))
							 (usocket:socket-close master-socket)
							 (node-log instance "Exiting...")))
						 :name "Server thread"))))

(defun node-log (node control-string &rest arguments)
  (apply #'locked-format `(,(node-output node)
						   ,(str:concat "[S] " control-string "~%") ,@arguments)))

(defun delete-closed-connections (node)
  (with-slots (nodes-inbound) node
	(setf nodes-inbound
		  (loop :for connection :in nodes-inbound
				:unless (should-stop connection)
				  :collect connection
				:do (bt:join-thread (node-connection-thread connection))))))

(defun all-nodes (node)
  (with-slots (nodes-inbound nodes-outbound) node
	(append nodes-inbound nodes-outbound)))

(defvar *connections-count* 0)

(defmethod initialize-instance :after ((instance node-connection) &key)
  (with-slots (thread socket id should-stop main-node) instance
	(setf thread (bt:make-thread
				  (lambda ()
					(node-log main-node "New client!")
					(handler-case (process-socket instance socket)
					  (end-of-file (e)
						(declare (ignore e))
						(node-log main-node "Someone left the server")
						(setf should-stop t)
						(usocket:socket-close socket))))
				  :name id))))

(defun generate-id ()
  (format nil "LN~d" (1- (incf *connections-count*))))

(defun process-socket (connection socket)
  (with-slots (thread main-node should-stop id) connection
	(unwind-protect
		 (loop :until should-stop
			   :do (let ((line (socket-timeout-read-line socket 10.0)))
					 (when line
					   (locked-format t "<~a>~a~%" id line))))
	  (node-log main-node "Closing connection...")
	  (usocket:socket-close socket))))

(defun stop-node-connection (connection)
  (setf (should-stop connection) t))

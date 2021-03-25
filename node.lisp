;;;; node.lisp
;;; Abstraction over a node in a peer-to-peer network

(in-package #:wired)

(declaim (optimize (safety 2) (debug 3)))

(defparameter *output-lock* (bt:make-lock))

(defun locked-format (stream control-string &rest arguments)
  "Use lock to access *standart-output*"
  (bt:with-lock-held (*output-lock*)
	(apply #'format `(,stream ,control-string ,@arguments))))

(defun socket-stream-format (stream control-string &rest args)
  "Force the output after format"
  (apply #'format `(,stream ,control-string ,@args))
  (force-output stream))

(defun socket-send-buffer (socket data)
  (let ((s (usocket:socket-stream socket)))
	(write-sequence data s)
	(force-output s)))

(defclass atomic-event ()
  ((value :initform nil
		  :type boolean
		  :reader event-get)
   (lock :initform (bt:make-lock))))

(defun event-set (evt)
  (with-slots (lock value) evt
	(bt:with-lock-held (lock)
	  (setf value t))))

(defun event-unset (evt)
  (with-slots (lock value) evt
	(bt:with-lock-held (lock)
	  (setf value nil))))

(defclass node ()
  ((server-thread :type bt:thread)
   (master-socket :reader node-server-socket)
   (connection-class :reader node-connection-class
					 :initarg :connection-class
					 :initform 'node-connection)
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
   (nodes-outbound :initform nil
				   :reader nodes-outbound)
   (nodes-lock :initform (bt:make-lock "nodes-lock")
			   :reader nodes-lock))
  (:documentation "A node connected to a larger P2P network"))

(defclass node-connection ()
  ((thread :type bt:thread
		   :reader node-connection-thread)
   (main-node :initarg :main-node
			  :initform (error "Must specify main node!")
			  :type node)
   (socket :initarg :socket
		   :initform (error "Must specify socket!")
		   :reader node-connection-socket)
   (should-stop :initform (make-instance 'atomic-event)))
  (:documentation "Class used to describe a connection to another node in the network"))

(defmethod host ((connection node-connection))
  (ip-address-to-string
   (usocket:get-peer-address (node-connection-socket connection))))

(defmethod port ((connection node-connection))
  (usocket:get-peer-port (node-connection-socket connection)))

(define-condition connecting-to-myself () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "You cannot connect to your own server!"))))

(defun socket-timeout-read (socket timeout)
  "Read from socket with timeout. Returns NIL if time's out."
  (when (usocket:wait-for-input `(,socket) :timeout timeout :ready-only t)
	(let* ((stream (usocket:socket-stream socket))
		   (line (read-line stream)))
	  (when line line))))

(defmethod initialize-instance :after ((node node) &key)
  (with-slots (server-thread output host port
			   master-socket nodes-inbound connection-class)
	  node
	(labels ((create-server ()
			   (restart-case (usocket:socket-listen host port :reuse-address t)
				 (change-port (new-port)
				   (setf port new-port)
				   (create-server))))
			 (server-main ()
			   (node-log node "Starting server on port ~d..." port)
			   (unwind-protect
					(loop (push (make-instance connection-class
											   :main-node node
											   :socket (usocket:socket-accept
														master-socket
														:element-type 'character))
								nodes-inbound))
				 (dolist (connection (all-nodes node))
				   (stop-node-connection connection))
				 (usocket:socket-close master-socket)
				 (node-log node "Exiting..."))))
	  (setf output *standard-output*
			master-socket (handler-bind ((usocket:address-in-use-error
										   (lambda (c)
											 (declare (ignore c))
											 (invoke-restart 'change-port (1+ port)))))
							(create-server))
			server-thread (bt:make-thread #'server-main :name "Server thread")))))

(defun host-connected-p (node host port)
  "Checks if we're already connected to this node"
  (let ((formated-host (ip-address-to-string (usocket:get-host-by-name host))))
	(some (lambda (n) (and (equal (host n) formated-host)
					  (equal (port n) port)))
		  (all-nodes node))))

(defun connect-to-node (node host port)
  "Connect to the node at host:port. Don't forget to close it!"
  (delete-closed-connections node)
  (with-slots (nodes-inbound nodes-outbound connection-class)
	  node
	(when (and (equal (host node) host)
			   (= (port node) port))
	  (error 'connecting-to-myself))
	(if (host-connected-p node host port)
		(locked-format t "Already connected to this node!~%")
		(handler-case (push (make-instance connection-class
										   :main-node node
										   :socket (usocket:socket-connect
													host port
													:timeout 5
													:element-type 'character))
							nodes-outbound)
		  (t (c)
			(declare (ignore c))
			(locked-format t "Failed to connect to node!~%"))))))

(defun node-log (node control-string &rest arguments)
  (apply #'locked-format `(,(node-output node)
						   ,(str:concat "[" (subseq (node-id node) 0 8) "] " control-string "~%") ,@arguments)))

(defun delete-closed-connections (node)
  "Remove connections that have been closed."
  (labels ((connection-list-remove (connection-list)
			 (remove-if #'should-stop connection-list)))
	(with-slots (nodes-inbound nodes-outbound nodes-lock)
		node
	  (bt:with-lock-held (nodes-lock)
		(setf nodes-inbound (connection-list-remove nodes-inbound)
			  nodes-outbound (connection-list-remove nodes-outbound))))))

(defun all-nodes (node)
  "Returns all connected nodes"
  (delete-closed-connections node)
  (with-slots (nodes-inbound nodes-outbound) node
	(append nodes-inbound nodes-outbound)))

(defun node-close-connection (connection)
  (with-slots (main-node socket should-stop)
	  connection
	(node-log main-node "Closing connection...")
	(usocket:socket-close socket)
	(event-set should-stop)))

(defun send-message-to (node connection data)
  "Sends message to node after checking if he was connected."
  (delete-closed-connections node)
  (with-slots (nodes-inbound nodes-outbound) node
	(if (or (member connection nodes-inbound)
			(member connection nodes-outbound))
		(socket-stream-format (usocket:socket-stream (node-connection-socket connection))
							  "~a~%" data)
		(error "Sending a message to an unknown host!"))))

(defgeneric recieve-message (node connection message)
  (:documentation "Called every time the node recives a message from another node"))

(defgeneric node-connection (node connection)
  (:documentation "Called every time we connect to a new node or a new node connects to us"))

(defgeneric node-deconnection (node connection)
  (:documentation "Called every time we get disconnected from a node or a node disconnects from us"))

(defmethod recieve-message ((node node) connection message)
  (locked-format t "New message: ~a~%" message))

(defmethod node-connection ((node node) connection)
  (node-log node "New peer connected!"))

(defmethod node-deconnection ((node node) connection)
  (node-log node "Node left the server!"))

(defvar *connections-count* 0)

(defmethod initialize-instance :after ((node node-connection) &key)
  (with-slots (thread socket should-stop main-node)
	  node
	(setf thread (bt:make-thread
				  (lambda ()
					(handler-bind ((end-of-file
									 (lambda (c)
									   (declare (ignore c))
									   (invoke-restart 'close-connection))))
					  (node-connection main-node node)
					  (process-socket node socket)))
				  :name (format nil "Client thread N~d" (incf *connections-count*))))))

(defun should-stop (connection)
  (with-slots (should-stop thread) connection
	(or (not (bt:thread-alive-p thread))
		(event-get should-stop))))

(defun stop-node-connection (connection)
  "Mark the connection as finished"
  (with-slots (should-stop) connection
	(event-set should-stop)))

(defun ip-address-to-string (address)
  (format nil "~{~a~^.~}" (coerce address 'list)))

(defun process-socket (connection socket)
  "Handle new connection to the server"
  (with-slots (thread main-node should-stop) connection
	(unwind-protect
		 (loop :until (event-get should-stop)
			   :do (restart-case
					   (let ((line (socket-timeout-read socket 10.0)))
						 (when line
						   (recieve-message main-node connection line)))
					 (close-connection ()
					   (event-set should-stop))))
	  (node-close-connection connection))))

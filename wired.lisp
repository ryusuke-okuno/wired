;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(defun base-16-encode (array)
  "Return the string of the encoded byte array in base 16"
  (declare (type (array (unsigned-byte 8)) array))
  (apply #'str:concat
		 (map 'list
			  (lambda (a)
				(let ((s (write-to-string a :base 16)))
				  (str:concat (str:repeat (- 2 (length s)) "0") s)))
			  array)))

(defun generate-id ()
  "Generate a random id for network identification"
  (base-16-encode (make-array 64 :element-type '(unsigned-byte 8)
								 :initial-contents (loop :repeat 64
														 :collect (the (unsigned-byte 8)
																	   (random 255))))))

(defclass wired-node (node)
  ((id :reader node-id
	   :initform (generate-id))
   (connection-class :initform 'wired-node-connection)
   (current-chain :accessor wired-chain
				  :initform (make-instance 'blockchain)))
  (:documentation "Node in the wired network"))

(defclass wired-node-connection (node-connection)
  ((id :accessor node-id
	   :initform "Default id")
   (server-port :initform 4444))
  (:documentation "Connection to a wired node"))

(defmethod port ((connection node-connection))
  (slot-value connection 'server-port))

(defmethod (setf port) (port (connection node-connection))
  (setf (slot-value connection 'server-port) port))

(defun valid-plist-string-p (string &rest indicators)
  (let ((parsed-plist (ignore-errors (read-from-string string))))
	(when (and parsed-plist
			   (every (lambda (id) (getf parsed-plist id))
					  indicators))
	  parsed-plist)))

(defun initialize-connection (node connection)
  (let* ((all-connections (all-nodes node))
		 (join-message (valid-plist-string-p
						(socket-timeout-read (node-connection-socket connection) 10.0)
						:id :server-port)))
	(when (null join-message)
	  (error 'wired-request-parsing-failed))
	(with-plist ((:id id)
				 (:server-port server-port))
		join-message
	  (when (some (lambda (x) (string= (node-id x) id))
				  (cons node all-connections))
		(error 'wired-request-parsing-failed))
	  (node-log node "Peer identified himself as ~a" id)
	  (setf (node-id connection) id
			(port connection) server-port))))

(defmethod node-connection ((node wired-node) connection)
  (socket-stream-format (usocket:socket-stream (node-connection-socket connection))
						"~a~%" (print-to-string (list :id (node-id node)
													  :server-port (port node))))
  (handler-case (initialize-connection node connection)
	(wired-request-parsing-failed ()
	  (node-log node "Peer failed to identify itself!")
	  (usocket:socket-close (node-connection-socket connection))
	  (stop-node-connection connection))))

(defmethod node-deconnection ((node wired-node) connection)
  (with-slots (nodes-inbound nodes-outbound) node
	(setf nodes-inbound (delete connection nodes-inbound)
		  nodes-outbound (delete connection nodes-outbound))))

(defmethod recieve-message ((node wired-node) connection message)
  (handler-case (handle-wired-request node connection message)
	(wired-request-parsing-failed ()
	  (node-log node "Parsing request failed!"))))

(define-condition wired-request-parsing-failed () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
             (format stream "Could not parse request!"))))

(defun print-to-string (obj)
  "Print object to the returned string"
  (str:replace-all "
" "" (with-output-to-string (s)
	   (print obj s))))

(defun wired-node-id-p (id)
  "Is the id a valid wired network id?"
  (declare (type string id))
  (= (length id) 128))

(defun make-wired-message (action content hops)
  (print-to-string (list :action action
						 :content content
						 :hops hops)))

(defun transmit-wired-message (node content hops)
  "If the current node is the destination, add a new post.
If it isn't, transmit it to the others nodes"
  (let ((destination-nodes (all-nodes node)))
	(dolist (connection destination-nodes)
	  (unless (member (node-id connection) hops :test #'equal)
		(node-log node "Sending to ~a" (node-id connection))
		(send-message-to node connection
						 (make-wired-message 'broadcast content
											 (append (mapcar #'node-id destination-nodes)
													 hops)))))))

(defun wired-broadcast (node message)
  "Send a message to the rest of the world."
  (transmit-wired-message node message (list (node-id node))))

(defun handle-wired-request (node connection message)
  "Handles a new sent request"
  (let ((parsed-message (ignore-errors
						 (let ((*package* (find-package :wired)))
						   (read-from-string message)))))
	(unless parsed-message (error 'wired-request-parsing-failed))
	(node-log node "Got message: ~a" parsed-message)
	(with-plist ((:action action))
		parsed-message
	  (unless action (error 'wired-request-parsing-failed))
	  (case action
		(broadcast
		 (with-plist ((:content content)
					  (:hops hops))
			 parsed-message
		   (when (or (null content) (null hops))
			 (error 'wired-request-parsing-failed))
		   (format t "=====ANON=====~%~a~%==============~%" content) ;Show that you recieved the message
		   (transmit-wired-message node content hops)))  ;And transmit it
		(get-peers
		 (send-message-to node connection
						  (make-wired-message 'send-peers
											  (mapcar (lambda (c) (list :host (host c)
																   :port (port c)))
													  (remove connection (all-nodes node)))
											  nil)))
		(send-peers
		 (with-plist ((:content hosts))
			 parsed-message
		   (mapcar (lambda (peer-plist)
					 (with-plist ((:host host)
								  (:port port))
						 peer-plist
					   (when (or (null host) (null port))
						 (error 'wired-request-parsing-failed))
					   (connect-to-node node host port)))
				   hosts)))
		(otherwise (error 'wired-request-parsing-failed))))))

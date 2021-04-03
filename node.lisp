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

(defun read-all (stream)
  (loop for char = (read-char-no-hang stream nil :eof)
		until (or (null char) (eq char :eof)) collect char into msg
		finally (return (values msg char))))

(defclass node (actor)
  ((server-thread :type bt:thread)
   (master-socket :reader node-server-socket)
   (connection-class :reader node-connection-class
					 :initarg :connection-class
					 :initform 'node-connection)
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
  ((main-node :initarg :main-node
			  :initform (error "Must specify main node!")
			  :type node)
   (socket :initarg :socket
		   :initform (error "Must specify socket!")
		   :reader node-connection-socket))
  (:documentation "Class used to describe a connection to another node in the network"))

(defmethod host ((connection node-connection))
  (ip-address-to-string
   (usocket:get-peer-address (node-connection-socket connection))))

(defmethod port ((connection node-connection))
  (usocket:get-peer-port (node-connection-socket connection)))

(defmethod node-id ((node node))
  "S")

(define-condition connecting-to-myself (error) ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "You cannot connect to your own server!"))))

(defun safe-close (socket)
  (ignore-errors (usocket:socket-close socket)))

(defun remove-node-connection (node connection)
  (with-slots (nodes-inbound nodes-outbound)
	  node
	(node-deconnection node connection)
	(safe-close (node-connection-socket connection))
	(setf nodes-inbound (delete connection nodes-inbound)
		  nodes-outbound (delete connection nodes-outbound))))

(defun node-new-connection (node)
  "Adds a new socket connection to the node"
  (with-slots (nodes-inbound) node
	(let ((new-connection (make-instance (node-connection-class node)
										 :main-node node
										 :socket (usocket:socket-accept
												  (node-server-socket node)
												  :element-type 'character))))
	  (handler-case (node-connection node new-connection)
		(t (c)
		  (declare (ignore c))
		  (node-log node "Failed to initialize connection!")
		  (remove-node-connection node new-connection)
		  (setf new-connection nil)))
	  (when new-connection
		(push new-connection nodes-inbound)))))

(defmethod initialize-instance :after ((node node) &key)
  (with-slots (server-thread output host port
			   master-socket nodes-inbound connection-class)
	  node
	(assert (subtypep connection-class 'node-connection))
	(labels ((create-server ()
			   (restart-case (usocket:socket-listen host port :reuse-address t)
				 (change-port (new-port)
				   (setf port new-port)
				   (create-server))))
			 (change-port (c)
			   (declare (ignore c))
			   (invoke-restart 'change-port (1+ port)))
			 (all-sockets ()
			   (cons master-socket (mapcar #'node-connection-socket (all-nodes node))))
			 (server-main ()
			   (node-log node "Starting server on port ~d..." port)
			   (unwind-protect
					(loop (loop :for sock :in (usocket:wait-for-input (all-sockets)
																	  :ready-only t
																	  :timeout 0)
								:do (if (eq sock master-socket)
										(node-new-connection node)
										(let ((connection (find sock (all-nodes node) :key #'node-connection-socket)))
										  (handler-case (process-connection connection)
											(t (c)
											  (node-log node "Closing connection: ~a..." c)
											  (remove-node-connection node connection))))))
						  (update-actor node)
						  (sleep 0.1))
				 (node-log node "Exiting...")
				 (dolist (connection (all-nodes node))
				   (safe-close (node-connection-socket connection)))
				 (safe-close master-socket))))
	  (setf master-socket (handler-bind ((usocket:address-in-use-error #'change-port))
							(create-server))
			server-thread (bt:make-thread #'server-main
										  :name (format nil "Node ~a" (sequence-take 8 (node-id node))))))))

(defun host-connected-p (node host port)
  "Checks if we're already connected to this node"
  (let ((formated-host (ip-address-to-string (usocket:get-host-by-name host))))
	(some (lambda (n) (and (equal (host n) formated-host)
					  (equal (port n) port)))
		  (all-nodes node))))

(defun connect-to-node (node host port)
  "Connect to the node at host:port. Don't forget to close it!"
  (with-slots (nodes-inbound nodes-outbound connection-class)
	  node
	(when (and (equal (host node) host)
			   (= (port node) port))
	  (error 'connecting-to-myself))
	(if (host-connected-p node host port)
		(locked-format t "Already connected to this node!~%")
		(handler-case (let ((new-connection
							  (make-instance connection-class
											 :main-node node
											 :socket (usocket:socket-connect
													  host port
													  :timeout 5
													  :element-type 'character))))
						(push new-connection nodes-outbound)
						(node-connection node new-connection))
		  (t (c)
			(declare (ignore c))
			(locked-format t "Failed to connect to node!~%"))))))

(defun node-log (node control-string &rest arguments)
  (apply #'locked-format
		 `(t ,(str:concat "[" (sequence-take 8 (node-id node)) "] " control-string "~%") ,@arguments)))

(defun all-nodes (node)
  "Returns all connected nodes"
  (with-slots (nodes-inbound nodes-outbound) node
	(append nodes-inbound nodes-outbound)))

(defun send-message-to (node connection data)
  "Sends message to node after checking if he was connected."
  (with-slots (nodes-inbound nodes-outbound) node
	(if (or (member connection nodes-inbound)
			(member connection nodes-outbound))
		(socket-stream-format (usocket:socket-stream (node-connection-socket connection))
							  "~a~%" data)
		(error "Sending a message to an unknown host!"))))

(defgeneric node-id (node)
  (:documentation "Returns the node's id"))

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

(defun ip-address-to-string (address)
  (format nil "~{~a~^.~}" (coerce address 'list)))

(defun process-connection (connection)
  "Handle new connection to the server"
  (let ((data (read-all (usocket:socket-stream (node-connection-socket connection)))))
	(when (<= (length data) 1)
	  (error "EOF when reading"))
	(with-slots (main-node socket) connection
	  (recieve-message main-node connection (coerce data 'string)))))

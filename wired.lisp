;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(defparameter *wired-version* "0.1")

(defun string-group (string n)
  (if (str:empty? string)
	  nil
	  (cons (str:substring 0 n string)
			(string-group (str:substring n nil string) n))))

(defun base-16-encode (array)
  "Return the string of the encoded byte array in base 16"
  (declare (type (array (unsigned-byte 8)) array))
  (apply #'str:concat
		 (map 'list
			  (lambda (a)
				(let ((s (write-to-string a :base 16)))
				  (str:concat (str:repeat (- 2 (length s)) "0") s)))
			  array)))

(defun base-16-decode (string)
  "Returns the decoded string data"
  (map 'vector
	   (lambda (x) (parse-integer x :radix 16))
	   (string-group string 2)))

(defun generate-id (host port)
  "Generate a random id using the host, port and a random number"
  (declare (type string host)
		   (type fixnum port))
  (let ((digest (ironclad:make-digest :sha512)))
	(ironclad:update-digest digest
							(string-to-utf-8-bytes
							 (str:concat host (write-to-string port)
										 (write-to-string (random 99999999)))))
	(base-16-encode (ironclad:produce-digest digest))))

(defclass wired-node (node)
  ((id :reader node-id)
   (connection-class :initform 'wired-node-connection))
  (:documentation "Node in the wired network"))

(defclass wired-node-connection (node-connection)
  ((id :accessor connection-id
	   :initform "Default id"))
  (:documentation "Specialization of node connection for the wired protocol"))

(defmethod initialize-instance :after ((instance wired-node) &key)
  (with-slots (host port id connection-class) instance
	(setf id (generate-id host port))))

(defmethod node-connection ((node wired-node) connection)
  (socket-stream-format (usocket:socket-stream (node-connection-socket connection))
						"~a~%" (node-id node))
  (let ((all-connections (all-nodes node))
		(sent-id (socket-timeout-read (node-connection-socket connection) 10.0)))
	(if (and sent-id (wired-node-id-p sent-id)
			 (not (member sent-id all-connections :key #'connection-id :test #'equal))
			 (not (string= sent-id (node-id node))))
		(progn
		  (node-log node "Peer identified himself as ~a" sent-id)
		  (setf (connection-id connection) sent-id))
		(progn
		  (node-log node "Peer failed to identify itself!")
		  (usocket:socket-close (node-connection-socket connection))
		  (stop-node-connection connection)))))

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
	  (unless (member (connection-id connection) hops :test #'equal)
		(node-log node "Sending to ~a" (connection-id connection))
		(send-message-to node connection
						 (make-wired-message 'broadcast content
											 (append (mapcar #'connection-id destination-nodes)
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
	(with-plist ((:action action)
				 (:content content)
				 (:hops hops))
		parsed-message
	  (when (or (some #'null (append (list action content)
									 (when (eq action 'broadcast) hops))))
		(error 'wired-request-parsing-failed))
	  (ecase action
		(broadcast						;Action broadcast, meaning: "send message to the whole network"
		 (format t "=====ANON=====~%~a~%==============~%" content) ;Show that you recieved the message
		 (transmit-wired-message node content hops))  ;And transmit it
		(get-peers
		 (send-message-to node connection
						  (make-wired-message 'send-peers
											  (mapcar (lambda (c) (list :host (host c)
																   :port (port c)))
													  (remove connection (all-nodes node)))
											  nil)))
		(send-peers
		 (mapcar (lambda (peer-plist)
				   (with-plist ((:host host)
								(:port port))
					   peer-plist
					 (when (or (null host) (null port))
					   (error 'wired-request-parsing-failed))
					 (connect-to-node node host port)))
				 content))
		(t (error 'wired-request-parsing-failed))))))

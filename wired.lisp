;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(defparameter *wired-version* "0.1")

(defun base-16-encode (array)
  "Return the string of the encoded byte array in base 16"
  (declare (type (array (unsigned-byte 8)) array))
  (apply #'str:concat
		 (map 'list
			  (lambda (a)
				(let ((s (write-to-string a :base 16)))
				  (str:concat (str:repeat (- 2 (length s)) "0") s)))
			  array)))

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

(defun get-wired-header (&optional (version *wired-version*))
  "Returns the obligatory header of the request"
  (format nil "WIRED/~d" version))

(defclass wired-node (node)
  ((id :reader node-id)
   (connections-table :initform (make-hash-table :test #'equal)
					  :accessor wired-connections-table))
  (:documentation "Node in the wired network"))

(defclass wired-post ()
  ((date :initarg :date
		 :initform (error "date!")
		 :reader post-date)
   (poster-id :initarg :poster-id
			  :initform (error "poster-id!")
			  :reader post-poster-id)
   (message :initarg :message
			:initform (error "message!")
			:reader post-message))
  (:documentation "A post in the wired board"))

(defmethod print-object ((object wired-post) stream)
  (format stream "<#WIRED-POST "))

(defparameter *board* (make-array 0 :element-type 'wired-post
									:adjustable t
									:fill-pointer t))

(defmethod initialize-instance :after ((instance wired-node) &key)
  (with-slots (host port id) instance
	(setf id (generate-id host port))))

(defun handle-wired-request (node text)
  "Handles a new sent request"
  (with-slots (id connections-table) node
	(let ((request (parse-wired-request text)))
	  (node-log node "Recieved ~a" request)
	  (ecase (getf request :action)
		(send (if (equal id (getf request :destination))
				  (vector-push-extend (read-from-string ()))))
		(t (error "Cannot handle this type of message yet!"))))))

(defmethod recieve-message ((node wired-node) connection message)
  (handler-case (handle-wired-request node message)
	(wired-request-parsing-failed ()
	  (node-log node "Parsing request failed!")
	  (send-message-to node connection
					   (forge-wired-request (list :version *wired-version*
												  :action 'request-not-understood))))))

(defmethod node-connection ((node wired-node) connection)
  (with-slots (connections-table id) node
	(socket-stream-format (usocket:socket-stream (node-connection-socket connection))
						  "~a~%" id)
	(let ((sent-id (socket-timeout-read (node-connection-socket connection) 10.0)))
	  (if (and sent-id (wired-node-id-p sent-id)
			   (null (gethash sent-id connections-table))
			   (not (string= sent-id id)))
		  (progn
			(node-log node "Peer identified himself as ~a" sent-id)
			(setf (gethash sent-id connections-table) connection)
			(call-next-method))
		  (progn
			(node-log node "Peer failed to identify itself!")
			(usocket:socket-close (node-connection-socket connection))
			(stop-node-connection connection))))))

(define-condition wired-request-parsing-failed () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
             (format stream "Could not parse request!"))))

(defparameter *wired-actions* '(("SEND" . (:name send :arguments (:destination :message)))
								("BROADCAST" . (:name broadcast :arguments (:message)))
								("GET" . (:name get :arguments ()))
								("REQUEST-NOT-UNDERSTOOD" . (:name request-not-understood :arguments ())))
  "List and informations about the different actions")

(defun wired-assoc-from-action (action)
  "Associates the action symbol to the name"
  (rassoc action *wired-actions*
		  :key (lambda (x) (getf x :name))))

(defun wired-request-p (plist)
  "Is plist a valid wired request?"
  (with-plist ((:action name)
			   (:version version))
	  plist
	(let ((plist-length (length plist)))
	  (and name version (evenp plist-length)
		   (loop :for arg :in (getf (cdr (wired-assoc-from-action name))
									:arguments)
				 :always (member arg plist))))))

(defun forge-wired-request (plist)
  "Transform plist to text request ready to be sent"
  (when (wired-request-p plist)
	(with-plist ((:action action)
				 (:version version))
		plist
	  (let ((action-assoc (wired-assoc-from-action action)))
		(str:concat (get-wired-header version)
					" " (car action-assoc) " "
					(str:join " "
							  (mapcar (lambda (argument) (getf plist argument))
									  (getf (cdr action-assoc) :arguments))))))))

(defun parse-wired-request (request)
  "Make plist from text wired request"
  (flet ((parse-assert (value)
		   (unless value (error 'wired-request-parsing-failed))))
	(let ((splitted (str:split " " request :omit-nulls t)))
	  (parse-assert (>= (length splitted) 2))
	  (destructuring-bind (head action &rest arguments) splitted
		(parse-assert (and (str:starts-with? "WIRED/" head)
						   (member action *wired-actions* :test #'equal :key #'car)))
		(let ((version (str:substring 6 nil head))
			  (action (cdr (assoc action *wired-actions* :test #'equal))))
		  (with-plist ((:name name)
					   (:arguments args))
			  action
			(parse-assert (same-size args arguments))
			(nconc (list :version version
						 :action name)
				   (mapcan #'list args arguments))))))))

(defun wired-node-id-p (id)
  (declare (type string id))
  (= (length id) 128))

;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(defun base-16-encode (array)
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

(defparameter *board* (make-array 0 :element-type 'wired-post
									:adjustable t
									:fill-pointer t))

(defmethod initialize-instance :after ((instance wired-node) &key)
  (with-slots (host port id) instance
	(setf id (generate-id host port))))

(defmethod recieve-message ((node wired-node) connection message)
  (node-log node "Hello, I guess?")
  (call-next-method))

(defmethod node-connection ((node wired-node) connection)
  (with-slots (connections-table id) node
	(socket-stream-format (usocket:socket-stream (node-connection-socket connection))
						  "~a~%" id)
	(let ((sent-id (socket-timeout-read (node-connection-socket connection) 10.0)))
	  (format t "Sent id: ~a~%" sent-id)
	  (if (and sent-id (wired-node-id-p sent-id)
			   (null (gethash sent-id connections-table)))
		  (progn
			(node-log node "Peer identified himself as ~a" sent-id)
			(setf (gethash sent-id connections-table) connection)
			(call-next-method))
		  (progn
			(node-log node "Peer failed to identify itself!")
			(usocket:socket-close (node-connection-socket connection))
			(stop-node-connection connection))))))

(defun wired-node-id-p (id)
  (declare (type string id))
  (= (length id) 128))

;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(defun generate-id (host port)
  "Generate a random id using the host, port and a random number"
  (declare (type string host)
		   (type fixnum port))
  (let ((digest (ironclad:make-digest :sha512)))
	(ironclad:update-digest digest
							(string-to-byte-array
							 (str:concat host (write-to-string port)
										 (write-to-string (random 99999999)))))
	(encode-base16 (ironclad:produce-digest digest))))

(defun encode-base16 (array)
  "Transform byte array into Base 16-encoded string"
  (declare (type (simple-array (unsigned-byte 8)) array))
  (apply #'str:concat
		 (map 'list
			  (lambda (a)
				(let ((number-string (write-to-string a :base 16)))
				  (str:concat (str:repeat (- 2 (length number-string)) "0")
							  number-string)))
			  array)))

(defun string-to-byte-array (string)
  "Get every character of a ASCII-encoded string into a byte array"
  (declare (type string string))
  (the (simple-array (unsigned-byte 8))
	   (make-array (length string)
				   :element-type '(unsigned-byte 8)
				   :initial-contents (loop :for c :across string
										   :collect (char-code c)))))

(defclass wired-node (node)
  ((id :reader node-id))
  (:documentation "Node in the wired network"))

(defclass wired-node-connection (node-connection)
  ((id :reader node-id))
  (:documentation "Connection to a node in the wired network"))

(defmethod initialize-instance :after ((instance wired-node) &key)
  (with-slots (host port id) instance
	(setf id (generate-id host port))))

(defmethod recieve-message ((node wired-node) connection message)
  (node-log node "Hello, I guess?")
  (call-next-method))

(defmethod node-connection ((node wired-node) connection)
  (with-slots (socket id) connection
	(socket-stream-format (usocket:socket-stream socket) "~a" id)
	(let ((sent-id (socket-timeout-read-line socket 10.0)))
	  (if (and sent-id
			   (wired-node-id-p sent-id))
		  (progn
			(setf id sent-id)
			(call-next-method))
		  (progn
			(node-log node "Peer failed to identify itself!")
			(usocket:socket-close socket)
			(stop-node-connection connection))))))

(defun wired-node-id-p (id)
  (declare (type string id))
  (= (length id) 128))

(in-package :wired)

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

(defclass wired-node (node)
  ()
  (:documentation ""))

(in-package :wired)

(defclass actor ()
  ((postbox :initform (queues:make-queue :simple-cqueue)
			:accessor postbox
			:documentation "The actor recieves the messages in this queue")
   (should-stop :initform nil
				:reader should-stop))
  (:documentation "Actor from the actor model"))

(defun update-actor (actor)
  (with-accessors ((postbox postbox)) actor
	(loop (multiple-value-bind (message not-empty)
			  (queues:qpop postbox)
			(if not-empty
				(destructuring-bind (method &rest arguments) message
				  (apply method actor arguments))
				(return))))))

(defun actor-send (actor function &rest args)
  (queues:qpush (postbox actor) `(,function ,@args)))

(defmacro actor-do (actor &body body)
  (alexandria:with-gensyms (actor-sym)
	`(let ((,actor-sym ,actor))
	   (actor-send ,actor-sym
				   (lambda (a)
					 (declare (ignore a))
					 ,@body)))))

(defmacro async-defun (name lambda-list &body body)
  (alexandria:with-gensyms (function-name)
	`(progn
	   (defun ,function-name ,lambda-list
		 ,@body)
	   (defun ,name ,lambda-list
		 (actor-send ,(car lambda-list) #',function-name ,@(cdr lambda-list))))))

(async-defun actor-stop (actor)
  (with-slots (should-stop) actor
	(setf should-stop t)))

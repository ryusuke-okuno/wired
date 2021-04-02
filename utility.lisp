(in-package :wired)

(defun longer (x y)
  "Is x longer than y?"
  (labels ((compare (x y)
			 (and (consp x)
				  (or (null y)
					  (compare (cdr x) (cdr y))))))
	(if (and (listp x) (listp y))
		(compare x y)
		(> (length x) (length y)))))

(defun same-size (x y)
  "Is the lenght of x the same as y?"
  (cond ((and (null x) (null y)) t)
		((or (null x) (null y)) nil)
		(t (same-size (cdr x) (cdr y)))))

(defun list-length-equal (lst n)
  "Is the length of list equal to n?
To not use length."
  (cond ((= n 0) (null lst))
		((null lst) (= n 0))
		(t (list-length-equal (cdr lst) (1- n)))))

(defmacro with-plist (bindings form &body body)
  "Binds plist slots to variables"
  (let ((form-symbol (gensym)))
	`(let ((,form-symbol ,form))
	   (let ,(mapcar (lambda (b)
					   (assert (list-length-equal b 2))
					   (list (second b)
							 `(getf ,form-symbol ,(first b))))
			  bindings)
		 ,@body))))

(defun take (n seq)
  (subseq seq 0 n))

(defun array-last (array)
  (when (> (length array) 0)
	(aref array (1- (length array)))))

(defun (setf array-last) (value array)
  (setf (aref array (1- (length array))) value))

(defun array-take (n array)
  (make-array n :displaced-to array))

(defun sequence-take (n seq)
  (subseq seq 0 (min (length seq) n)))

(declaim (inline take))

(defstruct (atomic (:type vector))
  (lock (bt:make-lock))
  (value (error "Must specify value!")))

(declaim (inline atomic-get))
(defun atomic-get (obj)
  "Get the contents of an atomic object"
  (atomic-value obj))

(defun (setf atomic-get) (value obj)
  "Set the contents of an atomic object"
  (bt:with-lock-held ((atomic-lock obj))
	(setf (atomic-value obj) value)))

(defmacro transform-error (original transformed form)
  `(handler-case ,form
	 (,original (c)
	   (declare (ignore c))
	   (error ',transformed))))

(defmacro doarray ((val array &optional index) &body body)
  (let ((array-sym (gensym "array"))
		(index-sym (or index (gensym "index"))))
	`(let ((,array-sym ,array))
	   (dotimes (,index-sym (length ,array-sym) ,array-sym)
		 (let ((,val (aref ,array-sym ,index-sym)))
		   ,@body)))))

(defmacro define-variables (&rest args)
  `(progn
	 ,@(loop :until (null args)
			 :collect `(defparameter ,(pop args) ,(pop args)))))

(defmacro in-new-thread (&body body)
  `(bt:make-thread (lambda () ,@body)))

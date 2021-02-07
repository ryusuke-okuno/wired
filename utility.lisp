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
					   (assert (= (length b) 2))
					   (list (second b)
							 `(getf ,form-symbol ,(first b))))
			  bindings)
		 ,@body))))

(in-package :wired)

(defparameter *node* (make-instance 'wired-node :port (+ 1000 (random 9999))))

(defun peers-count ()
  (format nil "Connected to ~d peers" (length (all-nodes *node*))))

(defun wired-connect ()
  (dolist (peer (get-regular-peers))
	(destructuring-bind (host port) peer
	  (connect-to-node *node* host port))))

(defun list-posts ()
  (reduce (lambda (last-block block)
			(str:concat last-block (format nil "======POST N~a======~%~a~%"
										   (block-id block) (block-contents block))))
		  (chain (node-blockchain *node*))
		  :initial-value ""))

(defun main ()
  (ltk:with-ltk ()
	(let* ((posting-frame (make-instance 'ltk:frame :borderwidth 10))
		   (post-text (make-instance 'ltk:text :master posting-frame
									 :width 50 :height 10))
		   (post-button (make-instance 'ltk:button
									   :text "Post"
									   :command (lambda ()
												  (wired-node-new-block *node* (ltk:text post-text))
												  (setf (ltk:text post-text) ""))
									   :master posting-frame))
		   (posts-label (make-instance 'ltk:label :text "No posts")))
	  (ltk:pack post-text)
	  (ltk:pack post-button)
	  (ltk:pack posting-frame :side :left)
	  (ltk:pack posts-label :side :right)
	  (labels ((update ()
				 (setf (ltk:text posts-label) (list-posts))
				 (ltk:after 5000 #'update)))
		(update)))))
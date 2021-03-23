(in-package :wired)

(defparameter *node* (make-instance 'wired-node :port 4445))

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
		  (reverse (chain (node-blockchain *node*)))
		  :initial-value ""))

(defun list-peers ()
  (format nil "Connected to ~a peers" (length (all-nodes *node*))))

(wired-connect)

(defparameter *ui-messages* (queues:make-queue :simple-cqueue))

(defun main ()
  (ltk:with-ltk ()
	(let* ((posting-frame (make-instance 'ltk:frame :borderwidth 10))
		   (post-text (make-instance 'ltk:text :master posting-frame
									 :width 50 :height 10))
		   (post-button (make-instance 'ltk:button
									   :text "Post"
									   :command (lambda ()
												  (let ((message (ltk:text post-text)))
													(setf (ltk:text post-text) "Calculating the proof of work...")
												    (bt:make-thread
													 (lambda ()
													   (wired-node-new-block *node* message)
													   (queues:qpush *ui-messages* 'calculated))
													 :name "Calculation thread")))
									   :master posting-frame))
		   (peers-label (make-instance 'ltk:label :text "Not connected"
												  :master posting-frame))
		   (posts-label (make-instance 'ltk:label :text "No posts")))
	  (ltk:pack peers-label)
	  (ltk:pack post-text)
	  (ltk:pack post-button)
	  (ltk:pack posting-frame :side :left)
	  (ltk:pack posts-label :side :right)
	  (get-chains-since (node-blockchain *node*) 1)
	  (get-more-peers *node*)
	  (labels ((update ()
				 (setf (ltk:text posts-label) (list-posts)
					   (ltk:text peers-label) (list-peers))
				 (multiple-value-bind (message not-empty)
					 (queues:qpop *ui-messages*)
				   (when not-empty
					 (case message
					   (calculated (setf (ltk:text post-text) ""))
					   (otherwise (format t "Wrong message!")))))
				 (ltk:after 1000 #'update)))
		(update)))))

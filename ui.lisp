(in-package :wired)

(defvar *node* nil)

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

(defclass wired-ui (actor)
  ((posting-frame :accessor posting-frame)
   (post-text :accessor post-text)
   (post-button :accessor post-button)
   (peers-label :accessor peers-label)
   (posts-label :accessor posts-label))
  (:documentation "Window to interface with the user"))

(defun post-message (ui post-text)
  (let ((message (ltk:text post-text)))
	(setf (ltk:text post-text)
		  "Calculating the proof of work...")
	(in-new-thread
	  (let* ((t1 (get-universal-time))
			 (new-block (calculate-block (node-blockchain *node*) message)))
		(actor-send *node* #'wired-new-block new-block)
		(actor-do ui (setf (ltk:text post-text)
						   (format nil "Done in ~a ms~%"
								   (- (get-universal-time) t1))))))))

(defun main ()
  (unless *node*
	(setf *node* (make-instance 'wired-node :port 4444)))
  (wired-connect)
  (let ((ui (make-instance 'wired-ui)))
	(with-slots (posts-label peers-label post-text posting-frame post-button)
		ui
	  (ltk:with-ltk ()
		(setf posting-frame (make-instance 'ltk:frame :borderwidth 10)
			  post-text (make-instance 'ltk:text :master posting-frame
												 :width 50 :height 10)
			  post-button (make-instance 'ltk:button
										 :text "Post"
										 :command (lambda ()	(post-message ui post-text))
										 :master posting-frame)
			  peers-label (make-instance 'ltk:label :text "Not connected"
													:master posting-frame)
			  posts-label (make-instance 'ltk:label :text "No posts"))
		(ltk:pack peers-label)
		(ltk:pack post-text)
		(ltk:pack post-button)
		(ltk:pack posting-frame :side :left)
		(ltk:pack posts-label :side :right)
		(get-chains-since (node-blockchain *node*) 1)
		(labels ((update ()
				   (setf (ltk:text posts-label) (list-posts)
						 (ltk:text peers-label) (list-peers))
				   (update-actor ui)
				   (ltk:after 1000 #'update)))
		  (update))))))

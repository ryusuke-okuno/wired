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

(defclass ui-node (wired-node)
  ((ui :initarg :ui
		:accessor node-ui)))

(defclass wired-ui (actor)
  ((posting-frame :accessor posting-frame)
   (post-text :accessor post-text)
   (post-button :accessor post-button)
   (peers-label :accessor peers-label)
   (posts-label :accessor posts-label)
   (infos-label :accessor infos-label)
   (calculating-block :initform nil
					  :accessor calculating-block))
  (:documentation "Window to interface with the user"))

(async-defun ui-post-calculated (ui t1)
  (setf (ltk:text (post-text ui))
		(format nil "Done in ~a ms~%"
				(- (get-universal-time) t1))
		(calculating-block ui) nil))

(defmethod wired-new-block ((node ui-node) new-block &key time)
  (handler-case
	  (progn
		(call-next-method)
		(ui-post-calculated (node-ui node) time))
	(adding-invalid-block (c)
	  (declare (ignore c))
	  (format t "Failed to add block, recalculating~%")
	  (post-message (node-ui node)
					(block-contents new-block)))))

(defun post-message (ui post-text)
  (let ((message (ltk:text post-text)))
	(if-not (calculating-block ui)
			(progn
			  (setf (ltk:text post-text)
					"Calculating the proof of work..."
					(calculating-block ui) t)
			  (in-new-thread
				(let* ((t1 (get-universal-time))
					   (new-block (calculate-block (node-blockchain *node*) message)))
				  (actor-send *node* #'wired-new-block new-block :time t1))))
			(setf (ltk:text post-text) "Please wait until the proof of work has been calculated..."))))

(defun stop-node ()
  (actor-stop *node*)
  (setf *node* nil))

(defun main ()
  (let ((ui (make-instance 'wired-ui)))
	(unless *node* (setf *node* (make-instance 'ui-node
											   :port 4444
											   :ui ui)))
	(wired-connect)
	(with-slots (posts-label peers-label post-text
				 posting-frame post-button infos-label)
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
			  infos-label (make-instance 'ltk:label :text "Getting chain from peers..."
													:master posting-frame)
			  posts-label (make-instance 'ltk:label :text "No posts"))
		(ltk:pack infos-label)
		(ltk:pack peers-label)
		(ltk:pack post-text)
		(ltk:pack post-button)
		(ltk:pack posting-frame :side :left)
		(ltk:pack posts-label :side :right)
		(labels ((update ()
				   (setf (ltk:text posts-label) (list-posts)
						 (ltk:text peers-label) (list-peers))
				   (update-actor ui)
				   (ltk:after 1000 #'update)))
		  (update))))))

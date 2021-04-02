;;;; wired.lisp
;;; Implementing the underlying principles of the wired network
;;; on top of the node abstraction.

(in-package #:wired)

(setf *random-state* (make-random-state t))

(defun base-16-encode (array)
  "Return the string of the encoded byte array in base 16"
  (declare (type (array (unsigned-byte 8)) array))
  (apply #'str:concat
		 (map 'list
			  (lambda (a)
				(let ((s (write-to-string a :base 16)))
				  (str:concat (str:repeat (- 2 (length s)) "0") s)))
			  array)))

(defun generate-id ()
  "Generate a random id for network identification"
  (base-16-encode (make-array 64 :element-type '(unsigned-byte 8)
								 :initial-contents (loop :repeat 64
														 :collect (the (unsigned-byte 8)
																	   (random 255))))))

(defclass wired-block (chain-block) ()
  (:documentation "A block of the wired blockchain"))

(defmethod initialize-instance :after ((chain-block wired-block) &key)
  (when (null (block-contents chain-block))
	(error "Must specify contents of the first block")))

(defmethod print-object ((chain-block wired-block) stream)
  (format stream "#<WIRED-BLOCK N~d~%	Proof of work: ~a~%	Contents: ~a~%	Hash: ~a>"
		  (block-id chain-block) (proof-of-work chain-block)
		  (block-contents chain-block) (base-16-encode (hash chain-block))))

(defmethod encode-block ((chain-block wired-block) &optional proof-of-work)
  (declare (ignore proof-of-work))
  (concatenate 'vector
			   (string-to-utf-8-bytes (block-contents chain-block))
			   (call-next-method)))

(defun wired-block-to-plist (wired-block)
  (with-accessors ((proof proof-of-work)
				   (contents block-contents)
				   (id block-id))
	  wired-block
	(list :id id
		  :proof-of-work proof
		  :contents contents)))

(defclass wired-blockchain (blockchain)
  ((block-class :initform 'wired-block)
   (node :initarg :node
		 :initform (error "Must specify node!")))
  (:documentation "The blockchain used to store the messages of the wired network"))

(defun wired-chain-to-plist (blockchain since-index)
  (with-accessors ((chain chain))
	  blockchain
	(let ((chain-slice-size (max 0 (- (length chain) since-index))))
	  (map 'vector #'wired-block-to-plist
		   (make-array chain-slice-size
					   :displaced-to chain
					   :displaced-index-offset (min (length chain) since-index))))))

(defclass wired-node (node)
  ((id :reader node-id
	   :initform (generate-id))
   (connection-class :initform 'wired-node-connection)
   (current-chain :accessor node-blockchain
				  :initform nil))
  (:documentation "Node in the wired network"))

(defclass wired-node-connection (node-connection)
  ((id :accessor node-id
	   :initform nil)
   (server-port :initform 4444))
  (:documentation "Connection to a wired node"))

(defmethod initialize-instance :after ((node wired-node) &key)
  (setf (node-blockchain node)
		(make-instance 'wired-blockchain
					   :content "Lisp is the most powerful programming language"
					   :content-pow 4611686018436821933
					   :node node)))

(defmethod port ((connection node-connection))
  (slot-value connection 'server-port))

(defmethod (setf port) (port (connection node-connection))
  (setf (slot-value connection 'server-port) port))

(defun valid-plist-string-p (string &rest indicators)
  (let ((parsed-plist (ignore-errors (read-from-string string))))
	(when (and (listp parsed-plist)
			   (every (lambda (id) (getf parsed-plist id))
					  indicators))
	  parsed-plist)))

(defun initialize-connection (node connection message)
  (let* ((all-connections (all-nodes node))
		 (join-message (valid-plist-string-p message
											 :id :server-port)))
	(when (null join-message)
	  (error 'wired-request-parsing-failed
			 :request join-message))
	(with-plist ((:id id)
				 (:server-port server-port))
		join-message
	  (when (some (lambda (x) (string= (node-id x) id))
				  (cons node all-connections))
		(error 'wired-request-parsing-failed
			   :request join-message))
	  (node-log node "Peer identified himself as ~a" id)
	  (setf (node-id connection) id
			(port connection) server-port)
	  (with-accessors ((blockchain node-blockchain))
		  node
		(if (< (length all-connections) 2)
			(get-chains-since (node-blockchain node) 0))))))

(defmethod node-connection ((node wired-node) connection)
  (socket-stream-format (usocket:socket-stream (node-connection-socket connection))
						"~a~%" (print-to-string (list :id (node-id node)
													  :server-port (port node)))))

(defmethod node-deconnection ((node wired-node) connection)
  (with-slots (nodes-inbound nodes-outbound) node
	(setf nodes-inbound (delete connection nodes-inbound)
		  nodes-outbound (delete connection nodes-outbound))))

(defmethod recieve-message ((node wired-node) connection message)
  (if (node-id connection)
	  (handler-case (handle-wired-request node connection message)
		(wired-request-parsing-failed (c)
		  (node-log node "Parsing request failed: ~a" c)))
	  (progn
		(handler-case (initialize-connection node connection message)
		  (wired-request-parsing-failed (c)
			(node-log node "Peer failed to identify itself: ~a" c)
			(usocket:socket-close (node-connection-socket connection))
			(remove-node-connection node connection)))
		(get-more-peers node))))

(define-condition wired-request-parsing-failed (error)
  ((request :initarg :request
			:initform nil
			:reader request))
  (:report (lambda (condition stream)
			 (if (request condition)
				 (format stream "Could not parse \"~a\"" (request condition))
				 (format stream "Could not parse request!")))))

(defun print-to-string (obj)
  "Print object to the returned string"
  (str:replace-all "
" "" (with-output-to-string (s)
	   (print obj s))))

(defun wired-node-id-p (id)
  "Is the id a valid wired network id?"
  (declare (type string id))
  (= (length id) 128))

(defun make-wired-message (action content hops)
  (print-to-string (list :action action
						 :content content
						 :hops hops)))

(defun transmit-wired-message (node content hops)
  "If the current node is the destination, add a new post.
If it isn't, transmit it to the others nodes"
  (let ((destination-nodes (remove-if (lambda (connection)
										(member (node-id connection) hops :test #'equal))
									  (all-nodes node))))
	(dolist (connection destination-nodes)
	  (node-log node "Sending to ~a" (node-id connection))
	  (send-message-to node connection
					   (make-wired-message 'broadcast content
										   (append (mapcar #'node-id destination-nodes) hops))))))

(defun wired-broadcast (node message)
  "Send a message to the rest of the world."
  (transmit-wired-message node message (list (node-id node))))

(defconstant +max-peers+ 20)

(defun get-more-peers (node)
  "Ask for more peers to all of the neightboring nodes"
  (when (< (length (all-nodes node)) +max-peers+)
	(dolist (n (all-nodes node))
	  (send-message-to node n (make-wired-message 'get-peers nil nil)))))

(defmacro with-plist-error (bindings form &body body)
  "Checks every element of the plist before using it"
  (alexandria:with-gensyms (form-sym)
	`(let ((,form-sym ,form))
	   (with-plist ,(mapcar (alexandria:curry #'take 2) bindings) ,form-sym
		 (unless (and ,@(loop :for b :in bindings
							  :when (null (caddr b))
								:do (error "Must specify test function!")
							  :collect `(funcall ,(caddr b) ,(cadr b))))
		   (error 'wired-request-parsing-failed
				  :request ,form-sym))
		 ,@body))))

(defun wired-new-broadcast (node message)
  "Recieved new block from peers"
  (node-log node "Recieved message: ~a" message)
  (with-plist-error ((:id id #'numberp)
					 (:proof-of-work proof #'numberp)
					 (:contents contents #'stringp))
	  message
	(transform-error adding-invalid-block wired-request-parsing-failed
					 (add-block (node-blockchain node)
								(make-instance 'wired-block
											   :contents contents
											   :proof-of-work proof
											   :id id
											   :previous-hash (hash (array-last (chain (node-blockchain node))))
											   :blockchain (node-blockchain node))))))

(defun wired-new-block (node new-block)
  "Add block to chain and send it to others"
  (with-accessors ((blockchain node-blockchain))
	  node
	(add-block blockchain new-block)
	(wired-broadcast node (wired-block-to-plist new-block))))

(defmethod get-chains-since ((blockchain wired-blockchain) index)
  (with-slots (node) blockchain
	(dolist (connection (all-nodes node))
	  (send-message-to node connection
					   (make-wired-message 'get-chain index nil)))))

(defmethod more-recent-block-p (chain-block (blockchain wired-blockchain))
  (> (length (chain blockchain))
	 (block-id chain-block)))

(defun get-regular-peers (&optional (path "peers.txt"))
  "Reads the file and gets back the peers"
  (mapcar (lambda (raw-line)
			(let ((pos (position #\: raw-line)))
			  (unless pos (error "Error in peers file!"))
			  (list (subseq raw-line 0 pos)
					(parse-integer (subseq raw-line (1+ pos))))))
		  (str:split "
" (alexandria:read-file-into-string path)
:omit-nulls t)))

(defun handle-wired-request (node connection message)
  "Handles a new sent request"
  (let ((parsed-message (ignore-errors
						 (let ((*package* (find-package :wired))
							   (*read-eval* nil))
						   (read-from-string message)))))
	(unless parsed-message
	  (error 'wired-request-parsing-failed :request message))
	(node-log node "Got message: ~a" parsed-message)
	(with-plist-error ((:action action #'symbolp))
		parsed-message
	  (case action
		(broadcast (with-plist-error ((:content content #'listp)
									  (:hops hops #'listp))
					   parsed-message
					 (wired-new-broadcast node content)
					 (transmit-wired-message node content hops)))
		(get-peers (let ((other-peers (remove connection (all-nodes node))))
					 (when other-peers
					   (send-message-to node connection
										(make-wired-message 'send-peers
															(mapcar (lambda (c) (list :host (host c)
																				 :port (port c)))
																	other-peers)
															nil)))))
		(send-peers (mapc (lambda (peer-plist)
							(with-plist-error ((:host host #'stringp)
											   (:port port #'numberp))
								peer-plist
							  (connect-to-node node host port)))
						  (getf parsed-message :content)))
		(get-chain (with-plist-error ((:content index (lambda (x)
														(and (numberp x)
															 (>= x 1)))))
					   parsed-message
					 (send-message-to node connection
									  (print-to-string (list :action 'send-chain
															 :content (wired-chain-to-plist (node-blockchain node) index)
															 :index index)))))
		(send-chain (with-plist-error ((:content new-chain #'vectorp)
									   (:index index #'numberp))
						parsed-message
					  (update-chain (node-blockchain node)
									(let ((chain (make-array 0 :adjustable t
															   :fill-pointer t)))
									  (doarray (new-block new-chain)
										(with-plist-error ((:proof-of-work proof #'numberp)
														   (:id id #'numberp)
														   (:contents contents #'stringp))
											new-block
										  (transform-error adding-invalid-block wired-request-parsing-failed
														   (vector-push-extend (make-instance 'wired-block
																							  :proof-of-work proof
																							  :id id
																							  :contents contents
																							  :previous-hash (let ((res
																													 (if (= id index)
																														 (hash (aref (chain (node-blockchain node))
																																	 (1- index)))
																														 (hash (aref chain (1- (- id index)))))))
																											   res)
																							  :blockchain (node-blockchain node))
																			   chain))))
									  chain)
									index)))
		(otherwise (error 'wired-request-parsing-failed
						  :request parsed-message))))))

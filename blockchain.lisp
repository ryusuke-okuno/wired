(in-package :wired)

(defclass chain-block ()
  ((id :reader block-id
	   :type (unsigned-byte 64)
	   :initarg :id
	   :initform (error "Must specify block id!"))
   (previous-hash :reader previous-hash
				  :initarg :previous-hash
				  :initform (error "Must specify previous hash!")
				  :type (simple-array (unsigned-byte 8) 32))
   (proof-of-work :accessor proof-of-work
				  :type (unsigned-byte 64)
				  :initarg :proof-of-work
				  :initform nil)
   (contents :initarg :contents
			 :initform nil
			 :reader block-contents)))

(defgeneric encode-block (chain-block)
  (:documentation "Transforms the block to a byte array"))

(defgeneric verify-contents (chain-block)
  (:documentation "Verifys the contents of the block"))

(defmethod encode-block ((chain-block chain-block))
  (concatenate 'vector
			   (previous-hash chain-block)
			   (cl-intbytes:int64->octets (block-id chain-block))
			   (cl-intbytes:int64->octets (proof-of-work chain-block))))

(defmethod verify-contents ((chain-block chain-block)) t)

(defun hash (chain-block)
  (with-slots (id proof-of-work) chain-block
	(let* ((digest (ironclad:make-digest :sha256))
		   (block-bytes (encode-block chain-block))
		   (raw-block (make-array (length block-bytes)
								  :initial-element 0
								  :element-type '(unsigned-byte 8))))
	  (dotimes (i (length block-bytes))
		(setf (aref raw-block i)
			  (aref block-bytes i)))
	  (ironclad:update-digest digest raw-block)
	  (ironclad:produce-digest digest))))

(defun valid-hash-p (hash)
  (every #'zerop (take 2 hash)))

(define-condition adding-invalid-block () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "Trying to add an invalid block to the blockchain!"))))

(defgeneric more-recent-block-p (chain-block)
  (:documentation "Has a more recent node been added to the global chain?"))

(defmethod more-recent-block-p ((chain-block chain-block))
  (declare (ignore chain-block)) nil)

(define-condition more-recent-block () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "A more recent block was added to the blockchain!"))))

(defmethod initialize-instance :after ((chain-block chain-block) &key)
  (with-slots (proof-of-work) chain-block
	(if proof-of-work
		(unless (valid-hash-p (hash chain-block))
		  (error 'adding-invalid-block))
		(progn (setf proof-of-work 0)
			   (loop :for hash = (hash chain-block)
					 :until (valid-hash-p hash)
					 :do (if (more-recent-block-p chain-block)
							 (error 'more-recent-block)
							 (incf proof-of-work)))))))

(defclass blockchain ()
  ((chain :accessor chain
		  :initform (make-array 0 :element-type 'chain-block
								  :adjustable t
								  :fill-pointer t))
   (block-class :initform 'chain-block
				:reader block-class))
  (:documentation "Blockchain class, that keeps a copy of every transaction and its hash"))

(defmethod initialize-instance :after ((blockchain blockchain) &key content)
  (vector-push-extend (make-instance (block-class blockchain)
									 :id 0
									 :previous-hash (make-array 64 :element-type '(unsigned-byte 8)
																   :initial-element 0)
									 :contents content)
					  (chain blockchain)))

(defgeneric get-chains-since (blockchain index)
  (:documentation "Ask for the blockchain since index to all peers"))

(defun calculate-block (blockchain &optional contents)
  "Calculate valid block to add to the blockchain"
  (with-accessors ((chain chain)
				   (class block-class))
	  blockchain
	(handler-case (make-instance class
								 :id (length chain)
								 :previous-hash (hash (array-last chain))
								 :contents contents)
	  (more-recent-block ()
		(format t "A new block has been added before us! Recalculating...~%")
		(calculate-block blockchain contents)))))

(defun update-chain (blockchain &optional (index 0))
  "Asks for the next blocks in the blockchain to peers after a
particular index, and takes the longest."
  (with-accessors ((chain chain))
	  blockchain
	(let* ((since-index (max 0 (- (length chain) index)))
		   (new-chain (reduce (lambda (last-chain chain)
								(if (> (length last-chain)
									   (length chain))
									last-chain chain))
							  (remove-if (complement #'verify-chain)
										 (get-chains-since blockchain since-index))))
		   (first-block (aref new-chain 0)))
	  (if (and (= (block-id first-block) since-index)
			   (equalp (hash (array-last chain))
					   (previous-hash first-block)))
		  (setf chain
				(make-array (+ since-index (length new-chain))
							:initial-contents (concatenate 'vector
														   (array-take since-index chain)
														   new-chain)
							:fill-pointer t
							:adjustable t))
		  (when (> (length new-chain) 4) ;We should probably trust this one
			(update-chain blockchain 4))))))

(defun add-block (blockchain chain-block)
  "Try to add a recived block to the chain"
  (labels ((verify-block (chain-block)
			 (equalp (previous-hash chain-block)
					 (hash (array-last (chain blockchain))))))
	(with-accessors ((chain chain))
		blockchain
	  (cond ((= (block-id chain-block) (length chain)) ;If everything is okay, add to the chain
			 (when (verify-block chain-block)
			   (vector-push-extend chain-block chain)))
			((> (block-id chain-block) (length chain)) ;If the block id is superior, it means we missed blocks
			 (update-chain blockchain)
			 (when (and (= (block-id chain-block) (length chain))
						  (verify-block chain-block))
			   (vector-push-extend chain-block chain)))
			(t (error 'adding-invalid-block))))))

(defun verify-chain (chain)
  "Verify chain order"
  (reduce (lambda (last-block current-block)
			(and last-block
				 (let ((last-hash (hash last-block)))
				   (when (and (equalp last-hash (previous-hash current-block))
							  (valid-hash-p last-hash)
							  (valid-hash-p (hash current-block))
							  (verify-contents current-block)
							  (= (block-id last-block)
								 (1- (block-id current-block))))
					 current-block))))
		  chain))

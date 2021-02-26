(in-package :wired)

(defun take (n seq)
  (subseq seq 0 n))

(defun array-last (array)
  (aref array (1- (length array))))

(defun (setf array-last) (value array)
  (setf (aref array (1- (length array))) value))

(declaim (inline take))

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

(defmethod encode-block ((chain-block chain-block))
  (concatenate 'vector
			   (previous-hash chain-block)
			   (cl-intbytes:int64->octets (block-id chain-block))
			   (cl-intbytes:int64->octets (proof-of-work chain-block))))

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
  (every #'zerop (take 3 hash)))

(define-condition adding-invalid-block () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "Trying to add an invalid block to the blockchain!"))))

(defmethod initialize-instance :after ((chain-block chain-block) &key)
  (with-slots (proof-of-work) chain-block
	(if proof-of-work
		(unless (valid-hash-p (hash chain-block))
		  (error 'adding-invalid-block))
		(progn (setf proof-of-work 0)
			   (loop :for hash = (hash chain-block)
					 :until (valid-hash-p hash)
					 :do (incf proof-of-work))))))

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
	(make-instance class
				   :id (length chain)
				   :previous-hash (hash (array-last chain))
				   :contents contents)))

(defun update-chain (blockchain &optional (index 0))
  "Asks for the next blocks in the blockchain to peers after a
particular index, and takes the longest."
  (with-accessors ((chain chain))
	  blockchain
	(let* ((new-chain (reduce (lambda (last-chain chain)
								(if (> (length last-chain)
									   (length chain))
									last-chain chain))
							  (remove-if (complement #'verify-chain)
										 (get-chains-since blockchain (max 0 (- (length chain) index))))))
		   (first-block (aref new-chain 0)))
	  (if (and (= (block-id first-block) (length chain))
			   (equalp (hash (array-last chain))
					   (previous-hash first-block)))
		  (setf chain (concatenate 'vector chain new-chain))
		  (when (> (length new-chain) 4) ;We should probably trust this one
			(update-chain blockchain 4))))))

(defun add-block (blockchain chain-block)
  "Try to add a recived block to the chain"
  (labels ((verify-block (chain-block)
			 (every #'= (previous-hash chain-block)
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
  (and (= (block-id (aref chain 0)) 0)
	   (reduce (lambda (last-block current-block)
				 (and last-block
					  (let ((last-hash (hash last-block)))
						(when (and (every #'= last-hash (previous-hash current-block))
								   (valid-hash-p last-hash)
								   (valid-hash-p (hash current-block))
								   (= (block-id last-block)
									  (1- (block-id current-block))))
						  current-block))))
			   chain)))

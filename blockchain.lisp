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

(defgeneric encode-block (chain-block &optional proof-of-work)
  (:documentation "Transforms the block to a byte array"))

(defgeneric verify-contents (chain-block)
  (:documentation "Verifys the contents of the block"))

(defmethod encode-block ((chain-block chain-block) &optional proof-of-work)
  (concatenate 'vector
			   (previous-hash chain-block)
			   (cl-intbytes:int64->octets (block-id chain-block))
			   (cl-intbytes:int64->octets (or proof-of-work (proof-of-work chain-block)))))

(defmethod verify-contents ((chain-block chain-block)) t)

(defun hash (chain-block &optional pow)
  (with-slots (id proof-of-work) chain-block
	(let* ((digest (ironclad:make-digest :sha256))
		   (block-bytes (encode-block chain-block (or pow proof-of-work)))
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

(defclass blockchain ()
  ((chain :accessor chain
		  :initform (make-array 0 :element-type 'chain-block
								  :adjustable t
								  :fill-pointer t))
   (block-class :initform 'chain-block
				:reader block-class))
  (:documentation "Blockchain class, that keeps a copy of every transaction and its hash"))

(defgeneric more-recent-block-p (chain-block blockchain)
  (:documentation "Has a more recent node been added to the blockchain?"))

(defmethod more-recent-block-p (chain-block (blockchain blockchain))
  (declare (ignore chain-block)) nil)

(define-condition more-recent-block () ()
  (:report (lambda (condition stream)
			 (declare (ignore condition))
			 (format stream "A more recent block was added to the blockchain!"))))

(defmethod initialize-instance :after ((chain-block chain-block)
									   &key (blockchain (error "Must specify blockchain")))
  (with-slots (proof-of-work) chain-block
	(let ((finished (make-atomic :value nil))
		  (more-recent-block (make-atomic :value nil)))
	  (labels ((calculate-proof-of-work (id size)
				 (let ((local-proof (floor (* id (/ 18446744073709551615 size)))))
				   (loop :for hash = (hash chain-block local-proof)
						 :until (or (valid-hash-p hash)
									(atomic-get finished)
									(atomic-get more-recent-block))
						 :do (if (more-recent-block-p chain-block blockchain)
								 (setf (atomic-get more-recent-block) t)
								 (incf local-proof)))
				   (unless (or (atomic-get finished)
							   (atomic-get more-recent-block))
					 (setf (atomic-get finished) t
						   proof-of-work local-proof)))))
		(unless proof-of-work
		  (bt:join-thread
		   (let ((cpu-count (serapeum:count-cpus)))
			 (car (loop :for x :below cpu-count
						:collect (bt:make-thread (lambda () (calculate-proof-of-work x cpu-count))
												 :name (format nil "Hashing thread ~a" x))))))
		  (when (atomic-get more-recent-block)
			(error 'more-recent-block)))))))

(defmethod initialize-instance :after ((blockchain blockchain) &key content content-pow)
  (assert (subtypep (block-class blockchain) 'chain-block))
  (vector-push-extend (make-instance (block-class blockchain)
									 :id 0
									 :previous-hash (make-array 64 :element-type '(unsigned-byte 8)
																   :initial-element 0)
									 :contents content
									 :proof-of-work content-pow
									 :blockchain blockchain)
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
								 :blockchain blockchain
								 :contents contents)
	  (more-recent-block ()
		(format t "A new block has been added before us! Recalculating...~%")
		(calculate-block blockchain contents)))))

(defconstant +chain-trust-length+ 5)

(defun update-chain (blockchain new-chain index)
  "The consensus algorithm on wich chain is the right one"
  (when (> (length new-chain) 0)
	(with-accessors ((chain chain))
		blockchain
	  (let ((first-block (aref new-chain 0)))
		(if (and (> (length new-chain) (- (length chain) index))
				 (= (block-id first-block) index)
				 (verify-chain new-chain)
				 (equalp (hash (aref chain (1- index)))
						 (previous-hash first-block)))
			(setf chain
				  (make-array (+ index (length new-chain))
							  :initial-contents (concatenate 'vector
															 (array-take index chain)
															 new-chain)
							  :fill-pointer t
							  :element-type 'chain-block
							  :adjustable t))
			(when (and (> (length new-chain) +chain-trust-length+)
					   (> index 1)) ;We should probably trust this one
			  (get-chains-since blockchain 1)))))))

(defun add-block (blockchain chain-block)
  "Try to add a recived block to the chain"
  (labels ((verify-block (chain-block)
			 (and (equalp (previous-hash chain-block)
						  (hash (array-last (chain blockchain))))
				  (valid-hash-p (hash chain-block)))))
	(with-accessors ((chain chain))
		blockchain
	  (cond ((= (block-id chain-block) (length chain)) ;If everything is okay, add to the chain
			 (when (verify-block chain-block)
			   (vector-push-extend chain-block chain)))
			((> (block-id chain-block) (length chain)) ;If the block id is superior, it means we missed blocks
			 (get-chains-since blockchain (length chain)))
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

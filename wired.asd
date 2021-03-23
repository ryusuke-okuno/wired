;;;; wired.asd

(asdf:defsystem #:wired
  :description "A decentralized communication protocol"
  :author "Phi"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
			   #:usocket
			   #:str
			   #:bt-semaphore
			   #:ironclad
			   #:trivial-utf-8
			   #:cl-intbytes
			   #:array-operations
			   #:ltk
			   #:serapeum
			   #:queues.simple-cqueue)
  :components ((:file "package")
			   (:file "utility")
			   (:file "blockchain")
			   (:file "node")
               (:file "wired")
			   (:file "ui")))

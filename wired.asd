;;;; wired.asd

(asdf:defsystem #:wired
  :description "A decentralized communication protocol"
  :author "Phi"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:usocket #:str #:bt-semaphore #:ironclad #:trivial-utf-8)
  :components ((:file "package")
			   (:file "node")
               (:file "wired")))

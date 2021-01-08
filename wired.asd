;;;; wired.asd

(asdf:defsystem #:wired
  :description "A decentralized communication protocol"
  :author "Phi"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:usocket #:str)
  :components ((:file "package")
               (:file "wired")))

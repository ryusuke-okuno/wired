;;;; package.lisp

(defpackage #:wired
  (:use #:cl)
  (:export #:create-server
		   #:connect-to-wired
		   #:is-host-up))

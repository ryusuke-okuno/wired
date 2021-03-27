(ql:quickload :wired)

(sb-ext:save-lisp-and-die "wired" :toplevel #'wired::main :executable t :compression 5)

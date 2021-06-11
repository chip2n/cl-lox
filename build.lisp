(asdf:load-system "lox")
(ql:quickload :lox)
(sb-ext:save-lisp-and-die "lox" :toplevel #'lox::main :executable t)

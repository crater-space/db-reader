(load "~/quicklisp/setup.lisp")
(ql:quickload "fiveam")

(load "./lib.lisp")
(load "./lib-tests.lisp")

(let ((result (run! 'lib-tests)))
  (cond
    ((null result)
     (uiop:quit 1))
    (t
     (uiop:quit))))

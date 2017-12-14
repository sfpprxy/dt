(use test posix)

(change-directory (car (command-line-arguments)))

(load "dt.ss")

(define dt (make-todo-manager))

(test-begin "dt")

(test 3 (+ 1 2))

(test-end "dt")

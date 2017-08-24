#! /usr/local/bin/csi -script

;; # list all
;;   => td [-l]
;; # add new item
;;   => td -a "Go for a walk "
;; # del one item
;;   => td -d id
;; # finish one item
;;   => td -f id
;; # redo one item
;;   => td -r id

(use uuid)
(require-extension srfi-13)

;; file to read & write
(define filename "./list")

;; => (get-item "title" (list "id" "status" "take breakfast"))
;; => "take breakfast"
(define (get-item name todo)
  (cond ((string=? name "id") (car todo))
        ((string=? name "status") (cadr todo))
        ((string=? name "title") (caddr todo))))

(define (color-print s c)
  (cond ((string=? "" c) (display (string-append "\033[0m" s "\033[0m")))
        ((string=? "red" c) (display (string-append "\033[1;31m" s "\033[0m")))
        ((string=? "blue" c) (display (string-append "\033[1;34m" s "\033[0m")))
        ((string=? "cyan" c) (display (string-append "\033[1;36m" s "\033[0m")))
        ((string=? "green" c) (display (string-append "\033[1;32m" s "\033[0m")))
        ((string=? "yellow" c) (display (string-append "\033[1;33m" s "\033[0m")))
        ))

;; => (print-todo (list 1 2 1 "take breakfast"))
;; => 
(define (print-todo todo)
  (color-print "     |  " "")
  (color-print (string-append (get-item "id" todo) ":") "")
  (color-print " " "")
  (color-print "[ " "")
  (if (string=? (get-item "status" todo) "0")
    (color-print "O" "red")
    (color-print "X" "green"))
  (color-print " ]" "")
  (color-print (string-append " - " (get-item "title" todo)) "")
  (newline))

;; return list of all todos
(define (all-todos) (read-file filename))

;; => (string-slice "test" 0 3)
;; => "tes"
(define (string-slice s start count)
  (if (> count 0)
    (string-append (string (string-ref s start)) (string-slice s (+ 1 start) (- count 1)))
    ""))

;; using uuid (https://github.com/dsosby/chicken-uuid)
(define (gen-id)
  (string-slice (uuid-v4) 0 3))

;; => (list-index (list 1 2 3) 1)
;; => 2
;; => (list-index (list 1 2 3) 3)
;; => ""
(define (list-index l i)
  (if (> (+ i 1) (length l))
    ""
    (list-ref l i)))

(define (list->string li)
  (define (merge-list-item l count)
    (if (null? l)
      ""
      (if (= count (- (length li) 1))
        (string-append "\"" (car l) "\"" (merge-list-item (cdr l) (+ 1 count)))
        (string-append "\"" (car l) "\" " (merge-list-item (cdr l) (+ 1 count))))))
  (string-append "(" (merge-list-item li 0) ")"))

(define (add-todo-to-file todo)
  (call-with-input-file filename
                        (lambda (input-port)
                          (let ([all (append (read-lines input-port) (list (list->string todo)))])
                            (write-all-to-file all)))))

(define (write-all-to-file all)
  (call-with-output-file filename
                         (lambda (output-port)
                           (for-each (lambda (l)
                                       (display l output-port)
                                       (display "\n" output-port)
                                       ) all))))

;; => (add-todo "go for a walk")
;; => Added!
(define (add-todo title)
  (let ([id (gen-id)]
        [status "0"])
    (add-todo-to-file (list id status title))
    (print "Added!")))

;; => (print-all)
;; => be42ea11-039a-4e6c-bdb3-400c66c39bb2: [ ] => go for a walk
;;    fc628735-112e-485c-378b-8cf6b7bf5593: [ ] => coffee
(define (print-all)
  (newline)
  (color-print "     --------" "")
  (newline)
  (let ([all (all-todos)])
    (if (> (length all) 0)
      (for-each print-todo all)
      (print "Empty list!")))
  (color-print "     --------" "")
  (newline)
  (newline))

;; print help
(define (print-help)
  (print "td: command is unknown")
  (print "td: '-l' show the list")
  (print "td: '-a' add a new item")
  (print "td: '-f' finish an item")
  (print "td: '-r' redo an item")
  (print "td: '-d id' delete id of an item"))

(define (match-todo id todo)
  (number? (string-contains todo id)))

;; delete an item
(define (delete-todo id)
  (define (del id todos)
    (cond ((null? todos) '())
          ((match-todo id (caar todos)) (del id (cdr todos)))
          (else (cons (car todos) (del id (cdr todos))))))
  (let ([all (map list->string (del id (all-todos)))])
    (write-all-to-file all)
    (print "Deleted!")))

(define (switch-status id status)
  (define (mark-todo todos)
    (cond ((null? todos) '())
          ((match-todo id (caar todos)) (cons (list (caar todos) status (caddar todos)) (mark-todo (cdr todos))))
          (else (cons (car todos) (mark-todo (cdr todos))))))
  (let ([all (map list->string (mark-todo (all-todos)))])
    (write-all-to-file all)
    ))

(define (get-title id todos)
  (cond ((null? todos) "")
        ((match-todo id (caar todos)) (caddar todos))
        (else (get-title (cdr todos)))))

(define (finish-todo id)
  (switch-status id "1")
  (print (string-append "Finished: " (get-title id (all-todos)))))
(define (redo-todo id)
  (switch-status id "0")
  (print (string-append "Redo: " (get-title id (all-todos)))))

;; main
((lambda (filename)
   (if (null? (command-line-arguments))
     (print-all)
     (let ([first-arg (list-index (command-line-arguments) 0)]
           [second-arg (list-index (command-line-arguments) 1)])
        (cond ((string=? "-l" first-arg) (print-all))
              ((string=? "-a" first-arg) (add-todo second-arg))
              ((string=? "-d" first-arg) (delete-todo second-arg))
              ((string=? "-f" first-arg) (finish-todo second-arg))
              ((string=? "-r" first-arg) (redo-todo second-arg))
              (else (print-help))
              )))
   ) filename)

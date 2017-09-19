#! /usr/local/bin/csi -script

(use redis-client)
(require-extension srfi-13)

(define VERSION "1.0.0")

;; db
(redis-connect "127.0.0.1" 6379)
(define (db-add k v)
  (redis-set k v))
(define (db-del k)
  (redis-del k))
(define (db-getallkeys)
  (redis-keys "*"))
(define (db-get k)
  (redis-get k))
(define (db-flush)
  (redis-flushall))

;; wrap a string with a color
(define (wrap-color s c)
  (cond ((null? c) (string-append "\033[0m" s "\033[0m"))
        ((eq? 'red c) (string-append "\033[1;31m" s "\033[0m"))
        ((eq? 'blue c) (string-append "\033[1;34m" s "\033[0m"))
        ((eq? 'cyan c) (string-append "\033[1;36m" s "\033[0m"))
        ((eq? 'green c) (string-append "\033[1;32m" s "\033[0m"))
        ((eq? 'yellow c) (string-append "\033[1;33m" s "\033[0m"))
        ((eq? 'light-gray c) (string-append "\033[1;30m" s "\033[0m"))
        ((eq? 'black c) (string-append "\033[0;30m" s "\033[0m"))))

;; indent spaces and display given strings
(define (indent-print . params)
  (display "     ")
  (for-each display params))

(define (print-todo id status text)
  (indent-print
    (wrap-color id 'light-gray)
    " [ "
    (if (string=? status "0")
      (wrap-color "X" 'red)
      (wrap-color "O" 'green))
    " ]"
    (wrap-color " - " 'light-gray)
    text
    "\n"))

;; generate id
(define (gen-id)
  (define (get-max-id keys max)
    (if (null? keys)
      max
      (let ([id (string->number
                  (car (string-split (car keys) ":")))])
        (if (> id max)
          (get-max-id (cdr keys) id)
          (get-max-id (cdr keys) max)))))
  (number->string (+ (get-max-id (db-getallkeys) 100) 1)))

;; => (list-index (list 1 2 3) 1)
;; => 2
;; => (list-index (list 1 2 3) 3)
;; => ""
(define (list-index l i)
  (if (> (+ i 1) (length l))
    ""
    (list-ref l i)))

;; => (add-todo "go for a walk")
;; => Added!
(define (add-todo text)
  (let ([id (gen-id)]
        [status "0"])
    (let ([k (string-append id ":" status)])
      (db-add k text)
      (newline)
      (indent-print "Added!\n")
      (newline))))

(define (print-all)
  (newline)
  (let ([keys (db-getallkeys)])
    (if (null? keys)
      (indent-print "Empty list!\n")
      (print-in-order (sort-by-id > keys '()))))
  (newline))

;; keys are '("id:status"...)
;; for instance: '("2:0" "3:1" "0:0" "1:0")
;; procedure sort-by-id takes a comparison procedure and a list, returns a new list sorted by id
;; this is using insertion sort
(define (sort-by-id p keys sorted)
  (define (insert l b)
    (if (null? l)
      (list b)
      (let ([id1 (string->number
                   (car
                     (string-split
                       (car l) ":")))]
            [id2 (string->number
                   (car
                     (string-split b ":")))])
        (if (p id1 id2)
          (cons (car l) (insert (cdr l) b))
          (cons b l)))))
  (if (null? keys)
    sorted
    (sort-by-id p (cdr keys) (insert sorted (car keys)))))

;; print finished items at last
(define (print-in-order keys #!optional (finished-count  0))
  (when (not (null? keys))
    (let ([text (car (db-get (car keys)))]
          [id (car (string-split (car keys) ":"))]
          [status (cadr (string-split (car keys) ":"))])
      (cond ((string=? status "1")
             (when (< finished-count 3)
               (print-todo id status text))
             (when (= finished-count 3)
               (indent-print ".....\n")
               (newline))
             (print-in-order (cdr keys) (+ finished-count 1)))
            (else
              (print-in-order (cdr keys) finished-count)
              (print-todo id status text))))))

;; print help
(define (print-help)
  (newline)
  (indent-print "Usage:\n")
  (indent-print "  => td [options] [args...]\n")
  (newline)
  (indent-print "Options:\n")
  (indent-print "  -a        Add an item           [string]\n")
  (indent-print "  -d        Delete an item by id  [string]\n")
  (indent-print "  -f        Finish an item by id  [string]\n")
  (indent-print "  -r        Redo an item by id    [string]\n")
  (indent-print "  -v        Print current version\n")
  (indent-print "  --clear   Clear all items\n")
  (newline))

(define (match-todo id todo)
  (number? (string-contains todo id)))

;; delete an item
(define (delete-todo id)
  (db-del (string-append id ":0"))
  (db-del (string-append id ":1"))
  (newline)
  (indent-print "Deleted!\n")
  (newline))

(define (switch-status id status)
  (let ([t1 (car (db-get (string-append id ":0")))]
        [t2 (car (db-get (string-append id ":1")))])
    (db-del (string-append id ":0"))
    (db-del (string-append id ":1"))
    (if (null? t1)
      (db-add (string-append id ":" status) t2)
      (db-add (string-append id ":" status) t1))))

(define (finish-todo id)
  (switch-status id "1")
  (newline)
  (indent-print "Finished: " (car (db-get (string-append id ":1"))) "\n")
  (newline))

(define (redo-todo id)
  (switch-status id "0")
  (newline)
  (indent-print "Redo: " (car (db-get (string-append id ":0"))) "\n")
  (newline))

(define (print-version)
  (newline)
  (indent-print "Version: " VERSION "\n")
  (newline))

(define (flush-todos)
  (db-flush)
  (newline)
  (indent-print "Cleared!\n")
  (newline))

;; main
((lambda ()
   (if (null? (command-line-arguments))
     (print-all)
     (let ([first-arg (list-index (command-line-arguments) 0)]
           [second-arg (list-index (command-line-arguments) 1)])
        (cond ((string=? "-l" first-arg) (print-all))
              ((string=? "-a" first-arg) (add-todo second-arg))
              ((string=? "-d" first-arg) (delete-todo second-arg))
              ((string=? "-f" first-arg) (finish-todo second-arg))
              ((string=? "-r" first-arg) (redo-todo second-arg))
              ((string=? "-v" first-arg) (print-version))
              ((string=? "--clear" first-arg) (flush-todos))
              (else (print-help)))))))

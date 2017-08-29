#! /usr/local/bin/csi -script

(use redis-client)
(require-extension srfi-13)

(define VERSION "0.1.0")

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

(define (color-print s c)
  (cond ((string=? "" c) (display (string-append "\033[0m" s "\033[0m")))
        ((string=? "red" c) (display (string-append "\033[1;31m" s "\033[0m")))
        ((string=? "blue" c) (display (string-append "\033[1;34m" s "\033[0m")))
        ((string=? "cyan" c) (display (string-append "\033[1;36m" s "\033[0m")))
        ((string=? "green" c) (display (string-append "\033[1;32m" s "\033[0m")))
        ((string=? "yellow" c) (display (string-append "\033[1;33m" s "\033[0m")))
        ))

(define (print-line #!optional [s (lambda () "")])
  (cond ((string? s)
         (color-print (string-append "     |  " s) ""))
        ((procedure? s)
         (color-print "     |  " "")
         (s))))

(define (print-todo id status text)
  (print-line (lambda ()
                (color-print (string-append id ":") "")
                (color-print " " "")
                (color-print "[ " "")
                (if (string=? status "0")
                  (color-print "X" "red")
                  (color-print "O" "green"))
                (color-print " ]" "")
                (color-print (string-append " - " text) "")
                (newline)
                )))

;; generate id
(define (get-max-id keys max)
  (if (null? keys)
    (number->string (+ max 1))
    (let ([id (string->number
                (car (string-split (car keys) ":")))])
      (if (> id max)
        (get-max-id (cdr keys) id)
        (get-max-id (cdr keys) max)))))
(define (gen-id)
  (get-max-id (db-getallkeys) 0))

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
      (print "    Added!")
      (newline))))

(define (print-all)
  (newline)
  (color-print "     --------" "")
  (newline)
  (print-line)
  (newline)
  (let ([keys (db-getallkeys)])
    (if (null? keys)
      (print-line (lambda ()
                    (color-print "Empty list!" "yellow")
                    (newline)))
      (print-in-order (sort-by-id > keys '()))))
  (print-line)
  (newline)
  (color-print "     --------" "")
  (newline)
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
               (print-line ".....")
               (newline)
               (print-line)
               (newline))
             (print-in-order (cdr keys) (+ finished-count 1)))
            (else
              (print-in-order (cdr keys) finished-count)
              (print-todo id status text))))))

;; print help
(define (print-help)
  (newline)
  (print "Usage:")
  (print "  => td [options] [args...]")
  (newline)
  (print "Options:")
  (print "  -a  Add an item           [string]")
  (print "  -d  Delete an item by id  [string]")
  (print "  -f  Finish an item by id  [string]")
  (print "  -r  Redo an item by id    [string]")
  (newline))

(define (match-todo id todo)
  (number? (string-contains todo id)))

;; delete an item
(define (delete-todo id)
  (db-del (string-append id ":0"))
  (db-del (string-append id ":1"))
  (newline)
  (print "    Deleted!")
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
  (print (string-append "    Finished: " (car (db-get (string-append id ":1")))))
  (newline))

(define (redo-todo id)
  (switch-status id "0")
  (newline)
  (print (string-append "    Redo: " (car (db-get (string-append id ":0")))))
  (newline))

(define (print-version)
  (newline)
  (print (string-append "    Version: " VERSION))
  (newline))

(define (flush-todos)
  (db-flush)
  (newline)
  (print (string-append "    Cleared!"))
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
              (else (print-help))
              )))))

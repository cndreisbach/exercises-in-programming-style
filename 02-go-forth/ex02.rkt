#lang racket

;; I tried to use the stack as much as possible. One thing I looked up is that ANS Forth
;; supports structs, so I allowed myself to use lists on the stack.

(define filename
  (if (= 0 (vector-length (current-command-line-arguments)))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(define stack '())
(define heap (make-hash))

(define (heap-set! k v)
  (hash-set! heap k v))

(define (heap-get k)
  (hash-ref heap k))

(define (push val)
  (set! stack (cons val stack)))

(define (push-list lst)
  (when (not (empty? lst))
    (push (first lst))
    (push-list (rest lst))))

(define (pop)
  (cond
    [(empty? stack) '()]
    [else (define val (first stack))
          (set! stack (rest stack))
          val]))

(define (peek)
  (if (empty? stack)
      (list)
      (first stack)))

(define (read-file)
  (define in (open-input-file (pop)))
  (push (port->string in))
  (close-input-port in))

(define (filter-chars)
  (push
   (string-trim
    (string-downcase
     (regexp-replace* #rx"[^A-Za-z]+" (pop) " ")))))

(define (scan)
  (push-list (string-split (pop))))

(define (remove-stop-words)
  (push "../stop_words.txt")
  (read-file)
  (heap-set! 'stop-words (string-split (string-trim (pop)) ","))
  (heap-set! 'words '())
  (let loop ()
    (when (not (empty? stack))
      (if (member (peek) (heap-get 'stop-words))
          (pop)
          (heap-set! 'words (cons (pop) (heap-get 'words))))
      (loop)))
  (push-list (heap-get 'words)))

(define (remove-short-words)
  (heap-set! 'words '())
  (let loop ()
    (when (not (empty? stack))
      (if (<= (string-length (peek)) 2)
          (pop)
          (heap-set! 'words (cons (pop) (heap-get 'words))))
      (loop)))
  (push-list (heap-get 'words)))

(define (frequencies)
  ;; pop all words off the stack
  ;; for each word
  ;; if stack is empty, push (word . 1)
  ;; if stack is not empty:
  ;;   pop first
  ;;   if first == word
  ;;     push (word . (inc1 (cdr first))
  ;;   else
  ;;     keep going
  ;;     push first
  (define (push-word word)
    (cond
      [(empty? stack) (push (cons word 1))]
      [else
       (define freq (pop))
       (cond [(equal? word (car freq))
              (push (cons word (add1 (cdr freq))))]
             [else
              (push-word word)
              (push freq)])]))
  
  (when (not (empty? stack))
    (define word (pop))
    (frequencies)
    (push-word word)))

(define (sort-stack)
  (define (sort-add e)
    (cond
      [(empty? stack) (push e)]
      [else
       (define t (pop))
       (cond
         [(> (cdr e) (cdr t))
          (push t)
          (push e)]
         [else
          (sort-add e)
          (push t)])]))
  (when (not (empty? stack))
    (define t (pop))
    (sort-stack)
    (sort-add t)))

(define (print-top-25)
  (for [(i (range 25))]
    (when (not (empty? stack))
      (define t (pop))
      (display (car t))
      (display " - ")
      (displayln (cdr t)))))

(push filename)
(read-file)
(filter-chars)
(scan)
(remove-stop-words)
(remove-short-words)
(frequencies)
(sort-stack)
(print-top-25)

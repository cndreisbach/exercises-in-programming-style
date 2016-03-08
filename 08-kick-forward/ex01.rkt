#lang racket

(require "../common.rkt")

(define stop-words (get-stop-words "../stop_words.txt"))

(define (read-file filename func)
  (func
   (with-input-from-file filename
     port->string)
   scan))

(define (filter-chars-and-normalize text func)
  (func
   (list->string (for/list ([c text])
                   (if (char-alnum? c)
                       (char-downcase c)
                       #\space)))
   remove-stop-words))

(define (scan text func)
  (func (string-split text) word-frequencies))

(define (remove-stop-words words func)
  (func
   (filter (lambda (word)
             (not (member word stop-words))) words)
   sort-frequencies))

(define (word-frequencies words func)
  (define word-freqs (make-hash))
  (for ([word words])
    (hash-update! word-freqs word add1 0))
  (func word-freqs print-word-freqs))

(define (sort-frequencies frequencies func)
  (func
   (sort (hash->list frequencies)
         (lambda (x y)
           (> (cdr x) (cdr y))))
   noop))

(define (print-word-freqs freqlist func)
  (for ([pair freqlist])
    (printf "~a - ~a~n" (car pair) (cdr pair)))
  (func '()))

(define noop (lambda (x) '()))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(read-file filename filter-chars-and-normalize)
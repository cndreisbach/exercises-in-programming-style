#lang racket

(require "../common.rkt")

(define (read-file filename)
  (with-input-from-file filename
    port->string))

(define (filter-chars-and-normalize text)
  (list->string (for/list ([c text])
                  (if (char-alnum? c)
                      (char-downcase c)
                      #\space))))

(define (scan text)
  (string-split text))

(define (remove-stop-words words stop-words)
  (filter (lambda (word)
            (not (member word stop-words))) words))

(define (word-frequencies words)
  (define word-freqs (make-hash))
  (for ([word words])
    (hash-update! word-freqs word add1 0))
  word-freqs)

(define (sort-frequencies frequencies)
  (sort (hash->list frequencies)
        (lambda (x y)
          (> (cdr x) (cdr y)))))

(define (print-pairs pairs)
  (for ([pair pairs])
    (printf "~a - ~a~n" (car pair) (cdr pair))))

(define (min-take xs c)
  (take xs (min c (length xs))))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(print-pairs
 (min-take
  (sort-frequencies
   (word-frequencies
    (remove-stop-words
     (scan
      (filter-chars-and-normalize
       (read-file filename))) (get-stop-words "../stop_words.txt")))) 25))
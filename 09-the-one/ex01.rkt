#lang racket

(require threading)
(require "../common.rkt")

(define one%
  (class object%
    (init value)
    (define current-value value)
    (super-new)

    (define/public (bind func)
      (set! current-value (func current-value))
      this)

    (define/public (printme)
      (print current-value)
      this)))

(define (read-file filename)
  (with-input-from-file filename
    port->string))

(define (filter-chars-and-normalize text)
  (list->string (for/list ([c text])
                  (if (char-alnum? c)
                      (char-downcase c)
                      #\space))))

(define stop-words (get-stop-words "../stop_words.txt"))

(define (remove-stop-words words)
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

(define (min-take c)
  (lambda (xs)
    (take xs (min c (length xs)))))

(define (print-pairs pairs)
  (for ([pair pairs])
    (printf "~a - ~a~n" (car pair) (cdr pair))))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(define the-one (new one% [value filename]))

(~> the-one
    (send bind read-file)
    (send bind filter-chars-and-normalize)
    (send bind string-split)
    (send bind remove-stop-words)
    (send bind word-frequencies)
    (send bind sort-frequencies)
    (send bind (min-take 25))
    (send bind print-pairs))

#lang racket

(define (string-prefix? s prefix)
  (equal? (substring s 0 (string-length prefix))
          prefix))

(define lower-case-letters
  (map string
       (map integer->char (range 97 123))))

(define (get-stop-words filename)
  (define stop-words-in (open-input-file filename))
  (define stop-words (string-split (port->string stop-words-in) ","))
  (append lower-case-letters stop-words))

(define (char-alnum? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))

(provide string-prefix?
         get-stop-words
         char-alnum?)
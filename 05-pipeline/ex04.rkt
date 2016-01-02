#lang racket

; In this exercise, I'm doing the word index task from the Prologue.
; Data structure for holding index:
;  hash of word -> list of page numbers

(require "../common.rkt")

;; Given a filename, return all lines
(define (read-file filename)
  (with-input-from-file filename
    port->lines))

;; Given a list of lines, filter and normalize each
(define (filter-chars-and-normalize lines)
  (for/list ([line lines])
    (list->string (for/list ([c line])
                    (cond [(char-alnum? c) (char-downcase c)]                        
                          [else #\space])))))

;; Given a list of lines, scan each for tokens
(define (scan lines)
  (for/list ([line lines])
    (string-split line)))

(define (remove-stop-words lines stop-words)
  (for/list ([words lines])
    (filter (lambda (word)
              (not (member word stop-words))) words)))

;; A page is made of page-size lines. Every time we see a word, we add that page to the word's index entry.
(define (create-index lines page-size)
  (define word-index (make-hash))
  (for ([line-count (in-naturals 1)]
        [line lines])
    (define page (ceiling (/ line-count page-size)))
    (for ([word line])
      (hash-update! word-index word (lambda (xs) (cons page xs)) (list))))
  (hash->list word-index))

;; Remove all entries with more than max-count occurances
(define (remove-frequent-entries word-index max-count)
  (filter (lambda (xs) (<= (length xs) (add1 max-count))) word-index))

(define (remove-duplicate-page-numbers word-index)
  (for/list ([entry word-index])
    (remove-duplicates entry)))

(define (print-index word-index)
  (for ([entry word-index])
    (printf "~a - ~a~n" (first entry) (string-join (map number->string (reverse (rest entry))) ", "))))

(define (sort-index word-index)
  (sort word-index string<? #:key first))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../pride-and-prejudice.txt"
      (vector-ref (current-command-line-arguments) 0)))

(print-index
 (sort-index
  (remove-duplicate-page-numbers
   (remove-frequent-entries
    (create-index
     (remove-stop-words
      (scan
       (filter-chars-and-normalize
        (read-file filename))) (get-stop-words "../stop_words.txt")) 45) 100))))
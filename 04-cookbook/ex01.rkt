#lang racket

(define (string-prefix? s prefix)
  (equal? (substring s 0 (string-length prefix))
          prefix))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
          "../input.txt"
          (vector-ref (current-command-line-arguments) 0)))

(define lower-case-letters
  (map string
       (map integer->char (range 97 123))))

(define (is-alnum? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))

(define data '())
(define words '())
(define word-freqs (make-hash))

(define (read-file)
  (with-input-from-file filename
    (lambda ()
      (set! data (append data (string->list (port->string)))))))

(define (filter-chars-and-normalize)
  (set! data
        (for/list ([c data])
          (if (is-alnum? c)
              (char-downcase c)
              #\space))))

(define (scan)
  (set! words
        (append words
                (string-split (list->string data)))))

(define (remove-stop-words)
  (define stop-words-in (open-input-file "../stop_words.txt"))
  (define stop-words (string-split (port->string stop-words-in) ","))
  (set! stop-words (append lower-case-letters stop-words))
  (set! words (filter (lambda (word)
                        (not (member word stop-words))) words)))

(define (frequencies)
  (for ([word words])
    (hash-update! word-freqs word add1 0)))

(define (sort-word-freqs)
  (set! word-freqs (sort (hash->list word-freqs)
                         (lambda (x y)
                           (> (cdr x) (cdr y))))))

(read-file)
(filter-chars-and-normalize)
(scan)
(remove-stop-words)
(frequencies)
(sort-word-freqs)

(for ([tf (take word-freqs (min 25 (length word-freqs)))])
  (display (car tf))
  (display " - ")
  (displayln (cdr tf)))

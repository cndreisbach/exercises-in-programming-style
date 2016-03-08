#lang racket

(require "../common.rkt")

(define text%
  (class object%
    (init filepath)
    
    (define text
      (with-input-from-file filepath
        port->string))
    
    (define normalized-text
      (list->string (for/list ([c text])
                      (if (char-alnum? c)
                          (char-downcase c)
                          #\space))))
    
    (super-new)

    (define/public (words)
      (string-split normalized-text))))

(define stop-words%
  (class object%
    (define stop-words (get-stop-words "../stop_words.txt"))

    (super-new)

    (define/public (stop-word? word)
      (member word stop-words))))

(define word-freqs%
  (class object%
    (define freqs (make-hash))

    (super-new)

    (define/public (increment word)
      (hash-update! freqs word add1 0))

    (define/public (sorted)
      (sort (hash->list freqs)
        (lambda (x y)
          (> (cdr x) (cdr y)))))))

(define word-freq-controller%
  (class object%
    (init filepath)

    (define text (new text% [filepath filepath]))
    (define stop-words (new stop-words%))
    (define word-freqs (new word-freqs%))

    (super-new)

    (define/public (run)
      (for ([word (send text words)])
        (when (not (send stop-words stop-word? word))
          (send word-freqs increment word)))

      (for ([pair (min-take (send word-freqs sorted) 25)])
        (printf "~a - ~a~n" (car pair) (cdr pair))))))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(send (new word-freq-controller% [filepath filename]) run)
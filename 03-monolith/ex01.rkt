#lang racket

;; defined to keep parity with Python version
(define (is-alnum? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))

(define filename
  (if (= 0 (vector-length (current-command-line-arguments)))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(define word-freqs '())
(define stop-words
  (with-input-from-file "../stop_words.txt"
    (lambda ()
      (string-split (port->string) ","))))
(set! stop-words (append stop-words '("a" "i" "o")))



(define in (open-input-file filename))
(define line (read-line in))
(let loop ()
  (when (not (eof-object? line))
    (define start-char '())
    (define i 0)
    (define found #f)
    (for [(c line)]
      (cond
        [(empty? start-char)
         (when (is-alnum? c)
           (set! start-char i))]
        [else
         (when (not (is-alnum? c))
           ;; we found the end of a word
           (set! found #f)
           (define word (string-downcase (substring line start-char i)))
           (when (not (member word stop-words))
             (for [(pair word-freqs)
                   #:break found]
               (when (equal? word (mcar pair))                      
                 (set-mcdr! pair (add1 (mcdr pair)))
                 (set! found #t)))
             (when (not found)
               (set! word-freqs (append word-freqs (list (mcons word 1))))))
           (set! start-char '()))])
      (set! i (add1 i)))
    (set! line (read-line in))
    (loop)))

(set! word-freqs (sort word-freqs (lambda (x y) (> (mcdr x) (mcdr y)))))

(for ([tf (take word-freqs (min 25 (length word-freqs)))])
  (display (mcar tf))
  (display " - ")
  (displayln (mcdr tf)))
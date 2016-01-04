#lang racket

(require "../common.rkt")

;; Tail-recursive functions

(define (count-words words stopwords)
  (let -count-words ([words words]
                     [stopwords stopwords]
                     [freqs (make-hash)])
    (cond
      [(empty? words) freqs]
      [else
       (when (not (member (first words) stopwords))
         (hash-update! freqs (first words) add1 0))
       (-count-words (rest words) stopwords freqs)])))

(define (print-word-freqs freqlist)
  (when (not (empty? freqlist))
    (define pair (first freqlist))
    (printf "~a - ~a~n" (car pair) (cdr pair))
    (print-word-freqs (rest freqlist))))

(define (scan chars)
  (let -scan ([chars chars]
              [current '()]
              [found '()])
    (cond
      [(empty? chars)
       (if (empty? current)
           found
           (cons (list->string current) found))]
      [(char=? (first chars) #\space)
       (if (empty? current)
           (-scan (rest chars) '() found)
           (-scan (rest chars) '() (cons (list->string (reverse current)) found)))]
      [else
       (-scan (rest chars) (cons (first chars) current) found)])))

;; Recursive (but not tail-recursive) functions

(define (filter-and-normalize chars)
  (if (empty? chars)
      '()
      (cons (filter-char (first chars)) (filter-and-normalize (rest chars)))))  

(define (take-max xs c)
  (cond
    [(empty? xs) '()]
    [(= c 0) '()]
    [else (cons (first xs) (take-max (rest xs) (sub1 c)))]))

;; Non-recursive functions

(define (read-file filename)
  (with-input-from-file filename
    (lambda () (port->list read-char))))

(define (filter-char c)
  (if (char-alnum? c)
      (char-downcase c)
      #\space))

(define (sort-frequencies frequencies)
  (sort (hash->list frequencies)
        (lambda (x y)
          (> (cdr x) (cdr y)))))

(define filename
  (if (or (= 0 (vector-length (current-command-line-arguments)))
          (string-prefix? (vector-ref (current-command-line-arguments) 0) "/var/folders"))
      "../input.txt"
      (vector-ref (current-command-line-arguments) 0)))

(print-word-freqs
 (take-max
  (sort-frequencies
   (count-words
    (scan
     (filter-and-normalize
      (read-file filename))) (get-stop-words "../stop_words.txt"))) 25))
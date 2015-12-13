#lang racket

;; In exercise 3, I used all my memory, but it did not seem to speed up my program much.


;; Overall strategy: (PART 1) read the input file, count the 
;; words, increment/store counts in secondary memory (a file) 
;; (PART 2) find the 25 most frequent words in secondary memory

(define _data (make-vector 1024 '()))
(define (data n)
  (vector-ref _data n))
(define (set-data! n x)
  (vector-set! _data n x))
(define (is-alnum? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))
(define (string-pad str size)
  (string-append str (make-string (- size (string-length str)) #\space)))


;; PART 1: 
;; - read the input file one line at a time
;; - filter the characters, normalize to lower case
;; - identify words, increment corresponding counts in file
(define stop-words-in (open-input-file "../stop_words.txt"))

;; data[0] contains stop words
(set-data! 0 (string-split (read-string 1024 stop-words-in) ","))
(close-input-port stop-words-in)

(set-data! 1 '())   ; data[1] is line
(set-data! 2 '())   ; data[2] is index of the start_char of word
(set-data! 3 0)     ; data[3] is index on characters, i = 0
(set-data! 4 #f)    ; data[4] is flag indicating if word was found
(set-data! 5 "")    ; data[5] is the word
(set-data! 6 "")    ; data[6] is word,NNNN
(set-data! 7 "")    ; data[7] is frequency
(set-data! 8 1024)  ; data[8] is the index of the current line

(define word-freqs-out (open-output-file "/tmp/wordfreqs" #:exists 'truncate))
(define word-freqs-in (open-input-file "/tmp/wordfreqs"))

(define input (open-input-file "pnp-2000.txt"))
;(define input (open-input-file "sampletext.txt"))

(let loop ()
  (when (= (data 8) 1024)
    (set-data! 8 9)
    (for [(i (range 9 1024))]
      (set-data! i '()))
    (set-data! 1 (read-line input 'any))
    (let load-data ()
      (when (and (not (eof-object? (data 1)))
                 (< (data 8) 1024))
        (set-data! 1 (string-append (data 1) "\n"))
        (set-data! (data 8) (data 1))
        (set-data! 8 (add1 (data 8)))
        (set-data! 1 (read-line input 'any))
        (load-data)))
    (set-data! 8 9))
  
  (when (and (< (data 8) 1024)
             (not (empty? (data (data 8)))))
    ;(set-data! 1 (data (data 8)))
    (set-data! 2 '())
    (set-data! 3 0)
    (for ([c (data (data 8))])
      (cond
        [(empty? (data 2))
         (when (is-alnum? c)
           ; We found the start of a word.
           (set-data! 2 (data 3)))]
        [else
         (when (not (is-alnum? c))
           ; We found the end of a word.
           (set-data! 4 #f)
           (set-data! 5 (string-downcase (substring (data (data 8)) (data 2) (data 3))))
           (when (and (>= (string-length (data 5)) 2)
                      (not (member (data 5) (data 0))))
             (set-data! 6 (read-line word-freqs-in))
             
             (let loop2 ()
               (when (not (eof-object? (data 6)))                 
                 (set-data! 7 (string->number (string-trim (second (string-split (data 6) ",")))))
                 (set-data! 6 (string-trim (first (string-split (data 6) ","))))
                 (cond
                   [(equal? (data 5) (data 6))
                    (set-data! 7 (add1 (data 7)))
                    (set-data! 4 #t)]
                   [else
                    (set-data! 6 (read-line word-freqs-in))
                    (loop2)])))             
             (cond
               [(not (data 4))
                (file-position word-freqs-out (file-position word-freqs-in))
                (display (string-pad (data 5) 20) word-freqs-out)
                (display "," word-freqs-out)
                (displayln (string-pad (number->string 1) 4) word-freqs-out)]
               [else
                ; Jump to beginning of last line read
                (file-position word-freqs-out (- (file-position word-freqs-in) 26))
                (display (string-pad (data 5) 20) word-freqs-out)
                (display "," word-freqs-out)
                (displayln (string-pad (number->string (data 7)) 4) word-freqs-out)])
             (flush-output word-freqs-out)
             (file-position word-freqs-in 0))
           (set-data! 2 '()))])
      (set-data! 3 (add1 (data 3))))
    (set-data! 8 (add1 (data 8)))
    (loop)))
(close-input-port input)
(close-output-port word-freqs-out)
(close-input-port word-freqs-in)

;; Part 2
;; Now we need to find the 25 most frequently occuring words.
;; We don't need anything from the previous values in memory.

(set! word-freqs-in (open-input-file "/tmp/wordfreqs"))
(set! _data (make-vector 27 '()))
(set-data! 25 "") ; data[25] is word,freq from file
(set-data! 26 0)  ; data[26] is freq

(set-data! 25 (read-line word-freqs-in 'any))
(let loop ()
  (when (not (eof-object? (data 25)))
    (set-data! 26 (string->number (string-trim (second (string-split (data 25) ",")))))
    (set-data! 25 (string-trim (first (string-split (data 25) ","))))
    (for ([i (range 25)]
          #:break (= (data 26) 0))
      (when (or (empty? (data i))
                (< (second (data i)) (data 26)))
        (set-data! i (list (data 25) (data 26)))
        (set-data! 26 0)))
    (set-data! 25 (read-line word-freqs-in 'any))
    (loop)))
(close-input-port word-freqs-in)

(for [(i (range 25))]
  (when (not (empty? (data i)))
    (display (string-pad (first (data i)) 20))
    (display " - ")
    (displayln (number->string (second (data i))))))


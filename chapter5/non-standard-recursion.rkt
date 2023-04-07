#lang htdp/isl+

(require 2htdp/image)

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [(zero? n) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))


; [List-of X] N -> [List-of [List-of X]]
(check-expect (list->chunks (list 1 2 3 4 5 6 7 8 9) 3)
              (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(check-expect (list->chunks (list 1 2 3 4 5 6 7 8 9) 4)
              (list (list 1 2 3 4) (list 5 6 7 8) (list 9)))
(check-expect (list->chunks (list 1 2 3 4 5 6 7 8 9) 5)
              (list (list 1 2 3 4 5) (list 6 7 8 9)))

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else
     (cons (take l n) (list->chunks (drop l n) n))]))


; String Number -> [List-of String]
; splits s into chunks of length n
(check-expect (partition "abcdefg" 1) (list "a" "b" "c" "d" "e" "f" "g" ""))
(check-expect (partition "abcdefg" 3) (list "abc" "def" "g"))
(check-expect (partition "abcdefg" 4) (list "abcd" "efg"))

(define (partition s n)
  (cond
    [(or (empty? s) (zero? n)) '()]
    [(< (string-length s) n) (list s)]
    [else
     (cons (substring s 0 n) (partition (substring s n (string-length s)) n))]))



; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< (list 1 2 3 4 5 6 7 8 9)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort< (list 9 8 7 6 5 4 3 2 1)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort< (list 8 3 1 9 2 4 7 5 6)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (sort< '()) '())

(define (quick-sort< alon)
    (cond
        [(empty? alon) '()]
        [else (local ((define pivot (first alon)))
                (append (quick-sort< (smallers (rest alon) pivot))
                        (list pivot)
                        (quick-sort< (largers (rest alon) pivot))))]))

; [List-of Number] Number -> [List-of Number]
; produces a list of all the numbers in alon that are larger than n
(define (largers alon n)
    (cond
        [(empty? alon) '()]
        [else (if (> (first alon) n)
                (cons (first alon) (largers (rest alon) n))
                (largers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
; produces a list of all the numbers in alon that are smaller than n
(define (smallers alon n)
    (cond
        [(empty? alon) '()]
        [else (if (< (first alon) n)
                (cons (first alon) (smallers (rest alon) n))
                (smallers (rest alon) n))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort<* alon)
    (cond
        [(empty? alon) '()]
        [(< (length alon) 10) (sort< alon)]
        [else (local ((define pivot (first alon)))
                (append (quick-sort<* (smallers alon pivot))
                        (list pivot)
                        (quick-sort<* (largers alon pivot))))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon using insertion sort
(check-expect (sort< (list 1 2 3 4 5 6 7 8 9)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (sort< (list 9 8 7 6 5 4 3 2 1)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (sort< (list 8 3 1 9 2 4 7 5 6)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (sort< '()) '())

(define (sort< alon)
  (local ((define (insert n lon)
            (cond
              [(empty? lon) (list n)]
              [(< n (first lon)) (cons n lon)]
              [else (cons (first lon) (insert n (rest lon)))])))
    (cond
        [(empty? alon) '()]
        [else (insert (first alon) (sort< (rest alon)))])))


(define MAX 10000)
(define SIZES (list 10 100 1000 10000))

(define (create-tests f los mx)
  (local ((define (run s)
            (time (f (build-list s (lambda (x) (random mx)))))))
    (map run los)))

; [List-of Number] -> Number
; calculates length of l
(check-expect (special1 (list 1 2 3 4 5 6 7 8 9)) 9)
(check-expect (special1 (list 1)) 1)
(check-expect (special1 '()) 0)

(define (special1 l)
  (local (
          (define (solve P) 0)
          (define (combine-solutions P processed)
            (add1 processed)))
    (cond
        [(empty? l) (solve l)]
        [else
         (combine-solutions l (special1 (rest l)))])))

; [List-of Number] -> [List-of Number]
; negates all the numbers in l
(check-expect (special2 (list 1 2 3 4 5 6 7 8 9)) (list -1 -2 -3 -4 -5 -6 -7 -8 -9))
(check-expect (special2 (list -1 -2 -3 -4 -5 -6 -7 -8 -9)) (list 1 2 3 4 5 6 7 8 9))
(check-expect (special2 '()) '())

(define (special2 l)
  (local (
          (define (solve P) '())
          (define (combine-solutions P processed)
            (cons (- P) processed)))
    (cond
        [(empty? l) (solve l)]
        [else
         (combine-solutions (first l) (special2 (rest l)))])))


; [List-of String] -> [List-of String]
; uppercases all the strings in l
(check-expect (special3 (list "a" "b" "c" "d" "e" "f" "g" "h" "i")) (list "A" "B" "C" "D" "E" "F" "G" "H" "I"))
(check-expect (special3 (list "abc" "def" "ghi")) (list "ABC" "DEF" "GHI"))
(check-expect (special3 '()) '())

(define (special3 l)
  (local (
          (define (solve P) '())
          (define (combine-solutions P processed)
            (cons (string-upcase P) processed)))
    (cond
        [(empty? l) (solve l)]
        [else
         (combine-solutions (first l) (special3 (rest l)))])))


(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))


(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S))
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

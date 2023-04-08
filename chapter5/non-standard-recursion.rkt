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


(define SMALL 4) ; a size measure in terms of pixels
(define small-triangle (triangle SMALL 'outline 'red))

; Number -> Image
; generative creates Sierpinski Δ of size side by generating
; one for (/ side 2) and placing one copy above two copies
(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))

(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized (beside half-sized half-sized)))]))


(define ε 0.001)
; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in
; one of the two halves, picks according to (2)
(check-within (find-root poly 1 3) 2 0.001)
(check-within (find-root poly 3 5) 4 0.001)
(check-satisfied (find-root poly 3 5) (lambda (n) (zero? (poly (round n)))))

(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))


; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))


(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))

; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= 0")))


(define table2 (make-table 1 a2))

(define table3 (make-table 10 (lambda (i) (- i 4))))

(define table4 (make-table 2 (lambda (i) (- i 4))))


; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

(define ERR-T2 "table2 is not defined for i =!= 0")

(define ERR-EMPTY "Empty table")

(define ERR-LIMIT "Length limit is reached")

(define ERR-NOT-FOUND "Root index is not found")

;; Table -> N
;; Finds the smallest index of a root
;; of the given monotonically increasing table.
(check-expect (find-linear table1) 0)
(check-error (find-linear table2) ERR-LIMIT)
(check-expect (find-linear table3) 4)
(check-error (find-linear table4) ERR-LIMIT)
(define (find-linear t)
  (local ((define len (table-length t))
          (define (find-linear* n)
            (cond
              [(= n len) (error ERR-LIMIT)]
              [else
               (if (zero? (table-ref t n))
                   n
                   (find-linear* (add1 n)))])))
    (find-linear* 0)))

;; Table -> N
;; Finds the smallest index of a root
;; of the given monotonically increasing table.
(check-error (find-binary (make-table 0 (lambda (i) i))) ERR-EMPTY)
(check-expect (find-binary (make-table 1 (lambda (i) i))) 0)
(check-expect (find-binary (make-table 2 (lambda (i) (- i 1)))) 1)
(check-expect (find-binary table1) 0)
(check-error (find-binary table2) ERR-NOT-FOUND)
(check-expect (find-binary table3) 4)
(check-error (find-binary table4) ERR-NOT-FOUND)
(define (find-binary t)
  (local ((define len (table-length t))
          (define (find-binary* l r)
            (local ((define (value-at n)
                      (table-ref t n))
                    (define f@l (value-at l))
                    (define f@r (value-at r))
                    (define distance (- r l)))
              (cond
                [(<= distance 1) (cond
                                   [(= f@l 0) l]
                                   [(= f@r 0) r]
                                   [else (error ERR-NOT-FOUND)])]
                [else
                 (local ((define mid (ceiling (/ (+ l r) 2)))
                         (define f@m (value-at mid)))
                   (cond
                     [(= f@m 0) mid]
                     [(> f@m 0) (find-binary* l mid)]
                     [else (find-binary* mid r)]))]))))
    (if (= 0 len)
        (error ERR-EMPTY)
        (find-binary* 0 (sub1 len)))))


; A File is one of:
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; interpretation represents the content of a file
; "\n" is the newline character

; A Line is a [List-of 1String].


; File -> [List-of Line]
; converts a file into a list of lines
(check-expect (file->list-of-lines
                (list "a" "b" "c" "\n"
                      "d" "e" "\n"
                      "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))


; File -> Line
; retrieves the prefix of afile up to the first occurrence of NEWLINE
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))


; File -> File
; drops the suffix of afile behind the first occurrence of NEWLINE
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))

(define NEWLINE "\n") ; the 1String

(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))
(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9 10 11 12))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)
       (list 10 11 12)))

(define (create-matrix n lst)
  (cond
    [(empty? lst) '()]
    [else
     (cons (take lst n)
           (create-matrix n (drop lst n)))]))

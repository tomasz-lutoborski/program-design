;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List -> Bool
; Checks if list contains "Flatt"
(check-expect (contains?
               (cons "A" (cons "Flatt" (cons "B" '())))) #true)
(check-expect (contains?
               (cons "A" (cons "F" (cons "B" '())))) #false)
(define (contains? list)
  (cond
    [(empty? list) #false]
    [(cons? list)
     (or (string=? (first list) "Flatt") (contains? (rest list)))]))

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
; example: (cons 10 (cons 20 (cons 30)))

; List-of-amounts -> Number
; sums all of amount on list
(check-expect (sum-amounts '()) 0)
(check-expect (sum-amounts (cons 10 (cons 20 (cons 30 '())))) 60)
(define (sum-amounts list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ (first list) (sum-amounts (rest list)))]))

; List-of-amounts -> Bool
; checks if list contains only positive values
(check-expect (pos? (cons 2 (cons 3 (cons 4 '())))) #true)
(check-expect (pos? (cons 2 (cons -3 (cons 4 '())))) #false)
(define (pos? list)
  (cond
    [(empty? list) #true]
    [(cons? list) (if (> 0 (first list)) #false (pos? (rest list)))]))


; List-of-bools -> Bool
; checks if list contains only positive values

(check-expect (all-true? (cons #true (cons #true '()))) #true)
(check-expect (all-true? (cons #true (cons #false '()))) #false)

(define (all-true? list)
  (cond
    [(empty? list) #true]
    [(cons? list) (if (first list) (all-true? (rest list)) #false)]))


; NEList-of-temperatures -> Number
; computes the average temperature 
 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))


; NEList-of-temperatures -> Number
; computes the sum of the given temperatures

(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))


; NEList-of-temperatures -> Bool
; checks if list is sorted in descending order

(check-expect
 (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect
 (sorted>? (cons 3 (cons 2 (cons 3 '())))) #false)
(check-expect
 (sorted>? (cons 3 '())) #true)
(check-expect
 (sorted>? (cons 10 (cons 5 (cons 1 '())))) #true)
(check-expect
 (sorted>? (cons 3 (cons 2 (cons 1 (cons 3 '()))))) #false)

(define (sorted>? nel)
  (cond
    [(empty? (rest nel)) #true]
    [else
     (cond
       [(> (first nel) (first (rest nel))) (sorted>? (rest nel))]
       [else #false])]))


; NEList-of-temperatures -> Number
; counts number of temparatures in list

(check-expect (how-many (cons 3 (cons 2 (cons 1 '())))) 3)
(check-expect (how-many (cons 5 (cons 4 (cons 3 (cons 2 (cons 1 '())))))) 5)

(define (how-many nel)
  (cond
   [(empty? (rest nel)) 1]
   [else (+ 1 (how-many (rest nel)))]))



















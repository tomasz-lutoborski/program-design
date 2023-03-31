;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* (list 1 2 3)) (list 2 3 4))

(define (add1* l)
  (plusn 1 l))


; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 (list 1 2 3)) (list 6 7 8))

(define (plus5 l)
  (plusn 5 l))


; Lon Number -> Lon
; adds n to each item on l
(check-expect (plusn 2 (list 1 2 3)) (list 3 4 5))

(define (plusn n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) n)
       (plusn n (rest l)))]))


; Nelon -> Number
; finds biggest or smallest number on
; non-empty list of numbers
(define (minmax R l)
  (cond
    [(empty? (rest l))
      (first l)]
     [else
      (if (R (first l)
             (minmax R (rest l)))
          (first l)
          (minmax R (rest l)))]))


; Nelon -> Number
; finds biggest number on the list
(check-expect (biggest (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)

(define (biggest l)
  (minmax > l))

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))


; Number -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list
(define (tabulate f n)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate f (sub1 n)))]))
	

; Number -> [List-of Numbers]
; tabulates sin between n and 0 (incl.) in a list
(define (tab-sin n)
  (tabulate sin n))


; Number -> [List-of Numbers]
; tabulates tan between n and 0 (incl.) in a list
(define (tab-tan n)
  (tabulate tan n))


; [List-of Item] -> Item
; folds the list with given function
(define (fold1 f l)
  (cond
    [(empty? l) 0]
    [else
     (f (first l)
        (fold1 f (rest l)))]))


; [List-of Number] -> Number
; computes the sum of the numbers on l
(check-expect (sum (list 1 2 3)) 6)

(define (sum l)
  (fold1 + l))


; [List-of Item] -> Item
; folds the list with given function
(define (fold2 f l on-empty)
  (cond
    [(empty? l) on-empty]
    [else
     (f (first l)
        (fold2 f (rest l) on-empty))]))


; [List-of Posn] -> Image
(define (image* l)
    (fold2 place-dot l emt))


; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

; [List-of Posn] -> [List-of Posn]
; add 3 to each x-coordinate
(check-expect (add-3-to-x
               (list (make-posn 1 1) (make-posn 2 2) (make-posn 3 3)))
              (list (make-posn 4 1) (make-posn 5 2) (make-posn 6 3)))

(define (add-3-to-x l)
  (local (; Posn -> Posn
          ; adds 3 to x-coordinate
            (define (add-3 p)
                (make-posn (+ 3 (posn-x p)) (posn-y p))))
    (map add-3 l)))

; [List-of Posn] -> [List-of Posn]
; eliminates all points with x-coordinate > 50
(check-expect (eliminate-x-gt-50
               (list (make-posn 20 30) (make-posn 40 50) (make-posn 60 70)))
              (list (make-posn 20 30) (make-posn 40 50)))

(define (eliminate-x-gt-50 l)
  (local (; Posn -> Boolean
          ; is x-coordinate > 50?
            (define (x-le-50? p)
                (< (posn-x p) 50)))
    (filter x-le-50? l)))

; [List-of Number] -> [List-of Number]
; converts list of euros to dollars
(check-expect (euros-to-dollars (list 1 2 3)) (list 1.2 2.4 3.6))

(define (euros-to-dollars l)
  (local (; Number -> Number
          ; converts euros to dollars
            (define (euro-to-dollar n)
                (* n 1.2)))
    (map euro-to-dollar l)))

; [List-of Posn] -> [List-of [List-of Number]]
; converts list of posns to list of lists of x and y coordinates
(check-expect (posns-to-xy (list (make-posn 1 2) (make-posn 3 4) (make-posn 5 6)))
              (list (list 1 2) (list 3 4) (list 5 6)))

(define (posns-to-xy l)
    (local (; Posn -> [List-of Number]
            ; converts posn to list of x and y coordinates
                (define (posn-to-xy p)
                    (list (posn-x p) (posn-y p))))
        (map posn-to-xy l)))

; Number [List-of Number] -> [List-of Number]
; eliminates all numbers on l that are bigger than n
(check-expect (eliminate-bigger-than 3 (list 1 2 3 4 5)) (list 1 2 3))

(define (eliminate-bigger-than n l)
  (local (; Number -> Boolean
          ; is n bigger than n?
            (define (bigger-than? m)
                (<= m n)))
    (filter bigger-than? l)))

; Name [List-of Name] -> Boolean
; is name or extension of name on list?
(check-expect (is-name-or-extension? "a" (list "a" "b" "c")) #t)
(check-expect (is-name-or-extension? "a" (list "b" "c" "d")) #f)
(check-expect (is-name-or-extension? "a" (list "ab" "bc" "cd")) #t)

(define (is-name-or-extension? name lon)
  (local (; Name -> Boolean
          ; is name or extension of name?
            (define (is-name-or-extension? n)
              (or (string=? name n)
                  (string=? name (substring n 0 (string-length name))))))
    (ormap is-name-or-extension? lon)))

; [X Y] [X Y -> Y] [List-of X] -> [List-of Y]
; maps f over l, starting with initial value
(check-expect (map-from-fold sub1 (list 1 2 3)) (list 0 1 2))

(define (map-from-fold f l)
  (local ((define (traverse x y) (cons (f x) y)))
    (foldr traverse '() l)))

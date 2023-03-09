;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname posn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Point -> Number
; computes distance of point from origin (0, 0)
(check-expect (dist (make-posn 3 4)) 5)
(check-expect (dist (make-posn 5 12)) 13)
(define (dist p)
  (sqrt
   (+
    (sqr (posn-x p))
    (sqr (posn-y p)))))

; Point -> Number
; computes distance of point from origin (0, 0) using manhattan distance
(check-expect (dist-manh (make-posn 3 4)) 7)
(check-expect (dist-manh (make-posn 5 12)) 17)
(define (dist-manh p)
  (+ (posn-x p) (posn-y p)))
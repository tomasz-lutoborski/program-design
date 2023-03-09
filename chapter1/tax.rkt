;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tax) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Price falls into one of three intervals: 
; — 0 through inclusive 1000
; — 1000 through inclusive 10000
; — 10000 and above.
; interpretation the price of an item

(define LOW-BOUNDARY 1000)
(define HIGH-BOUNDARY 10000)


; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 10000) (* 0.08 10000))
(check-expect (sales-tax 12017) (* 0.08 12017))

(define (sales-tax p)
  (cond
    [(and (>= p 0) (< p LOW-BOUNDARY)) 0]
    [(and (>= p LOW-BOUNDARY) (< p HIGH-BOUNDARY)) (* p 0.05)]
    [(>= p HIGH-BOUNDARY) (* p 0.08)]))
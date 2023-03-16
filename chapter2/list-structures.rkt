;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-structures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [info rate hours])
; A (piece of) Work is a structure: 
;   (make-work Info Number Number)
; interpretation (make-work n r h) combines the employee info 
; with the pay rate r and the number of hours h

(define-struct info [name number])
; A Info is a structure:
;  (make-info String Number)
; interpretation (make-info n num) combines employee
; name with employee number

(define-struct paycheck [employee payment])
; A Paycheck is a structure:
;  (make-paycheck String Number)
; interpretation (make-paycheck n p) combines
; the employee name with payment

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
 
(check-expect
  (wage* (cons (make-work (make-info "Robby" 1) 11.95 39) '()))
  (cons (make-paycheck (make-info "Robby" 1) (* 11.95 39)) '()))
 
(define (wage* an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons
                     (make-paycheck (work-info (first an-low)) (wage (first an-low)))
                     (wage* (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage w)
  (* (work-rate w) (work-hours w)))


; Posns-list -> Number
; sums x-coordinates of posns

(check-expect (sum-xs (cons (make-posn 10 10) (cons (make-posn 20 10) '()))) 30)

(define (sum-xs posns)
  (cond
    [(empty? posns) 0]
    [else (+ (posn-x (first posns)) (sum-xs (rest posns)))]))



















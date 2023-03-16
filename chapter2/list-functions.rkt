;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Currency-list -> Currency-list
; converts values in amounts
; by multiplying by given convertion rate

(check-expect (convert 2 (cons 10 (cons 5 '()))) (cons 20 (cons 10 '())))
(check-expect (convert 0.5 (cons 10 (cons 5 '()))) (cons 5 (cons 2.5 '())))

(define (convert rate amounts)
  (cond
    [(empty? amounts) '()]
    [else (cons (* (first amounts) rate) (convert rate (rest amounts)))]))


; String String String-list -> String-list
; substitute target by dest in list of Strings

(check-expect (substitute "red" "yellow" (cons "red" (cons "blue" '()))) (cons "yellow" (cons "blue" '())))

(define (substitute target dest list)
  (cond
    [(empty? list) '()]
    [(string=? (first list) target) (cons dest (substitute target dest (rest list)))]
    [else (cons (first list) (substitute target dest (rest list)))]))
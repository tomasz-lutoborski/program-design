;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; N String -> List-of-strings 
; creates a list of n copies of s
 
(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))
 
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))


; Number Number -> Number
; add m and n

(check-expect (add 4 5) 9)

(define (add m n)
  (cond
    [(zero? m) n]
    [else (add1 (add (sub1 m) n))]))


; Number Number -> Number
; multiplies n and m

(check-expect (mult 5 10) 50)
(check-expect (mult 1 20) 20)

(define (mult m n)
  (cond
    [(zero? n) 0]
    [(= m 1) n]
    [(positive? m) (add (mult (sub1 m) n) n)]))


; Number Image -> Image
; produces row of n images

(define (row n image)
  (cond
    [(= n 1) image]
    [else (above image (row (sub1 n) image))]))


; Number Image -> Image
; produces column of n images

(define (column n image)
  (cond
    [(= n 1) image]
    [else (beside image (column (sub1 n) image))]))


(define (create-scene m n)
  (place-image
   (row m (column n (square 20 "outline" "red")))
   100 100
   (empty-scene (+ 1 (* m 20)) (+ 1 (* n 20)))))

(define (add-ballons posns scene)
  (cond
    [(empty? posns) scene]
    [else
     (add-ballons
      (rest posns)
      (place-image
       (circle 3 "solid" "green")
       (posn-x (first posns)) (posn-y (first posns))
       scene))]))
















































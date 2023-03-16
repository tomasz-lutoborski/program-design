;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 300) ; distances in terms of pixels 
(define WIDTH 500)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))


; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate

; ShotWorld -> ShotWorld 
; moves each shot up by one pixel

(check-expect (tock (cons 10 (cons 9 '()))) (cons 9 (cons 8 '())))
(check-expect (tock (cons (+ HEIGHT 4) (cons 10 '()))) (cons 9 '()))
(check-expect (tock '()) '())

(define (tock w)
  (cond
    [(empty? w) '()]
    [(in-world? (first w)) (cons (sub1 (first w)) (tock (rest w)))]
    [else (tock (rest w))]))


; Number -> Bool
; checks if shot is in world boundaries

(check-expect (in-world? (+ 3 HEIGHT)) #false)
(check-expect (in-world? (- 3 HEIGHT)) #true)

(define (in-world? shot)
  (if (> shot HEIGHT) #false #true))


; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit

(check-expect (keyh (cons 9 '()) " ") (cons HEIGHT (cons 9 '())))
(check-expect (keyh (cons 9 '()) "a") (cons 9 '()))
(check-expect (keyh '() " ") (cons HEIGHT '()))

(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))


; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND

(check-expect (to-image (cons 10 (cons 9 '())))
              (place-image SHOT XSHOTS 10 (place-image SHOT XSHOTS 9 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))


; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))




























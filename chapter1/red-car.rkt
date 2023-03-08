;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname red-car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD (/ WIDTH-OF-WORLD 2))
     
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "blue"))

(define SPACE
  (rectangle (* WHEEL-RADIUS 4) (* 2 WHEEL-RADIUS) "solid" "red"))
(define BOTH-WHEELS
  (beside/align "baseline" WHEEL SPACE WHEEL))
(define CAR
  (above (rectangle (* WHEEL-RADIUS 2.5) WHEEL-RADIUS "solid" "red") BOTH-WHEELS))
(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
(define Y-CAR (- HEIGHT-OF-WORLD 10))

; WorldState is a Number
; interpretation: the number is distance from
; left border to car

; WorldState -> Image
; places the img x pixels from left border of the BACKGROUND
(define (render x)
  (place-image CAR (- x (image-width SPACE)) Y-CAR BACKGROUND))


; WorldState Number Number String -> WorldState
; places the car at the x-mouse is given me is "button-down"
(check-expect (mouse-move 40 10 10 "enter") 40)
(check-expect (mouse-move 40 10 10 "button-down") 10)

(define (mouse-move x-car x-mouse y-mouse me)
  (if (string=? me "button-down") x-mouse x-car))


; WorldState -> WorldState
; move the car by 3 pixels
(check-expect (tock 3) 4)
(check-expect (tock(tock 3)) 5)

(define (tock cw)
  (+ 1 cw))


; WorldState -> Bool
; checks if car is out of bonds of the world
(check-expect (end? 400) #true)
(check-expect (end? 30) #false)

(define (end? cw)
  (if (> cw WIDTH-OF-WORLD) #true #false))

; WorldState -> WorldState
; launches new world with initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [on-mouse mouse-move]
    [stop-when end?]))
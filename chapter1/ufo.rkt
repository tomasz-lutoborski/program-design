;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 300)
(define WIDTH 500)
(define TANK-WIDTH 50)
(define TANK-HEIGHT 25)
(define UFO-WIDTH 40)
(define UFO-HEIGHT 10)
(define MISSILE-SPEED 5)

(define SCENE (empty-scene WIDTH HEIGHT))


(define UFO (overlay/align "center" "bottom"
                           (circle UFO-HEIGHT "solid" "green")
                           (ellipse UFO-WIDTH UFO-HEIGHT "solid" "green")
                           ))

(define TANK (above/align "center"
                          (beside/align "center"
                                        (rectangle (/ TANK-WIDTH 2) (/ TANK-HEIGHT 2) "solid" "black")
                                        (rectangle 10 2 "solid" "black"))
                          (ellipse TANK-WIDTH (/ TANK-HEIGHT 2) "solid" "black")
                          ))

(define MISSILE (rectangle 2 10 "solid" "black"))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
     
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
     
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game


; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) HEIGHT im))
     
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image 
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

(define (si-game-over? s)
  (cond
    [(and (aim? s) (> (posn-y (aim-ufo s)) (- HEIGHT UFO-HEIGHT))) #true]
    [(and (fired? s)
          (and
           (= (posn-y (fired-missile s)) (posn-y (fired-ufo s)))
           (= (posn-x (fired-missile s)) (posn-x (fired-ufo s))))) #true]
    [else #false]))

; Tank -> Tank
; moves tank accordingly to its speed
(check-expect (move-tank (make-tank 10 5)) (make-tank 15 5))
(define (move-tank tank)
  (make-tank (+ (tank-loc tank) (tank-vel tank)) (tank-vel tank)))

; UFO -> UFO
; move UFO accordingly to its speed
(define (move-ufo ufo)
  (if (= (modulo (random 3) 3) 0)
      (if (= (random 1) 1) (make-posn (+ (posn-x ufo) (random 3)) (+ (posn-y ufo) 1))
          (make-posn (- (posn-x ufo) (random 3)) (+ (posn-y ufo) 1)))
      (make-posn (posn-x ufo) (+ (posn-y ufo) 1))))

; Missile -> Missile
; move missile accordingly to its speed
(define (move-missile missile)
  (make-posn (posn-x missile) (posn-y missile)))

; SIGS -> SIGS
; moves tank and ufo (and ew. missile) on tick
(define (move s)
  (cond
    [(aim? s)
     (make-aim
      (aim-ufo s)
      (move-tank (aim-tank s)))]
    [(fired? s)
     (make-fired
      (move-ufo (fired-ufo s))
      (move-tank (fired-tank s))
      (move-missile (fired-missile s)))]))


; SIGS -> Image
; renders the given game state on top of SCENE
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) SCENE))]
    [(fired? s)
     (tank-render
      (fired-tank s)
      (ufo-render (fired-ufo s)
                  (missile-render (fired-missile s)
                                  SCENE)))]))


(define (main s)
  (big-bang s
    [to-draw si-render]
    [stop-when si-game-over?]
    [on-tick move]))




























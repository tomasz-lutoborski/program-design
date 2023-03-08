;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname happiness-meter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 100)
(define HEIGHT-OF-WORLD (/ WIDTH-OF-WORLD 2))
(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define RECT-FRAME (rectangle WIDTH-OF-WORLD HEIGHT-OF-WORLD "outline" "black"))

; WorldHappiness is a Number
; interpretation: current happiness level

; WorldHappiness -> Image
; Draws rectangle of width expressing world happiness
(define (render wh)
  (overlay/align "left" "middle"
                 RECT-FRAME
                 (rectangle wh HEIGHT-OF-WORLD "solid" "red")))

; WorldHappiness -> WorldHappiness
; decreases WorldHappiness with ticks
(define (tock wh)
  (if (> wh 0.1) (- wh 0.1) wh))

; WorldHappiness Key -> WorldHappiness
; changes WorldHappiness if "up" or "down" key was pressed
(define (keypress wh key)
  (cond
    [(key=? key "up") (if (< (+ wh (* wh 1/3)) 100) (+ wh (* wh 1/3)) 100)]
    [(key=? key "down") (if (> (- wh (* wh 1/5)) 1) (- wh (* wh 1/5)) 1)]
    [else wh]))

; WorldHappiness -> WorldHappiness
; launches new world with initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-key keypress]
    [to-draw render]))

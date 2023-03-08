;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname first-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; constants
(define WIDTH  400)
(define HEIGHT  240)
(define V 10)

(define MTSCN  (empty-scene WIDTH HEIGHT)) ; short for empty scene 
(define ROCKET (square 40 "solid" "red"))
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))
 
; functions
(define (picture-of-rocket.v5 t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET (/ WIDTH 2) (distance t) MTSCN)]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET (/ WIDTH 2) ROCKET-CENTER-TO-TOP MTSCN)]))

(define (distance t)
         (* V t))

(animate picture-of-rocket.v5)
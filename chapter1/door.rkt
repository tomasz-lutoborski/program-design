;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname door) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

; DoorState is one of
; | LOCKED
; | CLOSED
; | OPEN


; DoorState -> DoorState
; closes open door
(check-expect (door-closer OPEN) CLOSED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer LOCKED) LOCKED)
(define (door-closer ds)
  (cond
    [(string=? OPEN ds) CLOSED]
    [(string=? CLOSED ds) CLOSED]
    [(string=? LOCKED ds) LOCKED]))

; DoorState KeyEvent -> DoorState
; turns key event into action on door
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action LOCKED "a") LOCKED)
(check-expect (door-action OPEN "a") OPEN)
(define (door-action ds k)
  (cond
    [(and (string=? LOCKED ds) (string=? "u" k))
     CLOSED]
    [(and (string=? CLOSED ds) (string=? "l" k))
     LOCKED]
    [(and (string=? CLOSED ds) (string=? " " k))
     OPEN]
    [else ds]))

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))
(define (door-render s)
  (text s 40 "red"))


; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
    [on-tick door-closer 3]
    [on-key door-action]
    [to-draw door-render]))
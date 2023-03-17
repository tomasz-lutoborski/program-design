;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dictionary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")


(define-struct lc [letter count])
; A LC (Letter-Count) is structure:
;  (make-lc 1String Number)
; interpretation: combines letter with number of words
; in dictionary starting with it

; A LoLC is one of:
; - '()
; - (cons LC LoLC)
(define list-of-letter-counts (cons (make-lc "a" 10) (cons (make-lc "b" 20) '())))

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))


; Letter Dictionary -> Number
; counts how many words in dictionary starts
; with given letter

(check-expect (starts-with# "a" (list "arbuz" "kot" "ala" "pies")) 2)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (length (filter-dict l d))]))


; Letter Dictionary -> Dictionary
; filter d to words only starting with l

(check-expect (filter-dict "a" (list "arbuz" "kot" "ala" "pies")) (list "arbuz" "ala"))
(check-expect (filter-dict "a" '()) '())

(define (filter-dict l d)
  (cond
    [(empty? d) '()]
    [(string=? (first (explode (first d))) l) (cons (first d) (filter-dict l (rest d)))]
    [else (filter-dict l (rest d))]))


; Dictionary Alphabet -> LoLC
; consumes dictionary and outputs how many words
; in dictionary starts with each letter from LETTERS

(define (count-by-letter ls d)
  (cond
    [(empty? ls) '()]
    [else (cons (make-lc (first ls) (starts-with# (first ls) d)) (count-by-letter (rest ls) d))]))


; LoLC -> LC
; given dictionary finds most frequent word with its count

(check-expect (most-frequent (cons (make-lc "a" 3) (cons (make-lc "b" 2) '()))) (make-lc "a" 3))
(check-expect (most-frequent (cons (make-lc "b" 3) (cons (make-lc "a" 2) '()))) (make-lc "b" 3))
(check-expect (most-frequent (cons (make-lc "a" 3) (cons (make-lc "b" 2) (cons (make-lc "c" 4) '())))) (make-lc "c" 4))

(define (most-frequent lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [(>= (lc-count (first lolc))
         (lc-count (second lolc)))
     (most-frequent (cons (first lolc) (rest (rest lolc))))]
    [else (most-frequent (rest lolc))]))



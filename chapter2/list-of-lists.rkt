;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-of-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 (require 2htdp/batch-io)

; LOS (short for list of strings) is one of: 
; – '()
; – (cons String LOS)
; interpretation an instance of Los represents
; list of Strings

; LL (list of lines) is one of:
; - '()
; - (cons LOS LL)

; LON (list of numbers) is one of:
; - '()
; - (cons Number LON)

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))

(define ln2 (cons '()
                  (cons "hello"
                        (cons '()
                              (cons (cons "my" (cons "world" '()))
                                    '())))))

(define ln3 (cons '()
                  (cons "hello"
                        (cons (cons "my" (cons "world" '()))
                              '()))))
  
; LL -> LON
; determines the number of words on each line 
 
(check-expect (words-on-line ln0) '())
(check-expect (words-on-line ln1) (cons 2 (cons 0 '())))
 
(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else (cons (length (first ln))
                (words-on-line (rest ln)))]))


; LL -> String
; converts lines into one string

(check-expect (collapse (cons '()
                              (cons "hello"
                                    (cons '()
                                          (cons (cons "my" (cons "world" '()))
                                                '()))))) "\nhello\nmy world")
(check-expect (collapse (cons '() (cons '() (cons '() '())))) "\n\n\n")

(define (collapse lines)
  (cond
    [(empty? lines) ""]
    [(empty? (first lines)) (string-append "\n" (collapse (rest lines)))]
    [(empty? (rest lines)) (collapse/line (first lines))]
    [else (string-append (collapse/line (first lines)) (collapse (rest lines)))]))


; LOS -> String
; converts one line into string

(check-expect (collapse/line (cons "hello" (cons "world" '()))) "hello world")
(check-expect (collapse/line '()) "")

(define (collapse/line line)
  (cond
    [(empty? line) ""]
    [(string? line) line]
    [(empty? (rest line)) (first line)]
    [else (string-append (first line) " " (collapse/line (rest line)))]))



; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))


; String -> String
; converts word using encode-letter

(check-expect (encode-word "abc") (string-append (encode-letter "a") (encode-letter "b") (encode-letter "c")))

(define (encode-word word)
  (cond
    [(= (string-length word) 0) ""]
    [else (string-append (encode-letter (first (explode word)))
                         (encode-word (implode (rest (explode word)))))]))


; LOS -> LOS
; converts line using encode-word

(check-expect (encode-line (cons "hello" (cons "world" '())))
              (cons (encode-word "hello") (cons (encode-word "world") '())))

(define (encode-line line)
  (cond
    [(empty? line) '()]
    [else (cons (encode-word (first line)) (encode-line (rest line)))]))


; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))


; LL -> LL
; encodes list of lines as three digits numbers

(define (encode file)
  (cond
    [(empty? file) '()]
    [else (cons (encode-line (first file)) (encode (rest file)))]))
































              

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)


(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define w1 (list "a" "b"))

; A List-of-words is one of:
; - '() or
; - (const Word List-of-words)
(define low1 (list (list "a" "b") (list "c" "d")))



; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings (list (list "a" "b") (list "c" "d"))) (list "ab" "cd"))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))


; List-of-strings -> List-of-words
; turns all Strings in low into Words
(check-expect (strings->words (list "ab" "cd"))
              (list (list "a" "b") (list "c" "d")))

(define (strings->words low)
  (cond
    [(empty? low) '()]
    [else (cons (string->word (first low)) (strings->words (rest low)))]))


; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary (list "chsadrdfas" "cat" "asdfvd" "bat")) (list "cat" "bat"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) AS-LIST) (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))


; 1String Word -> List-of-words
; inserts c in all possible places in word and returns this combinations
(check-expect (insert-everywhere/in-word "c" '() w1) (strings->words (list "cab" "acb" "abc")))

(define (insert-everywhere/in-word c pre post)
  (cond
    [(empty? post) (list (append pre (list c)))]
    [else (append (list (append pre (list c) post))
                  (insert-everywhere/in-word c (append pre (list (first post))) (rest post)))]))


; 1String List-of-words -> List-of-list-of-words
; inserts c in all possible places in all words
(check-expect (insert-everywhere/in-words "e" low1)
              (list
               (strings->words (list "eab" "aeb" "abe"))
               (strings->words (list "ecd" "ced" "cde"))))

(define (insert-everywhere/in-words c low)
  (cond
    [(empty? low) '()]
    [else (append
           (insert-everywhere/in-word c '() (first low))
           (insert-everywhere/in-words c (rest low)))]))



; Word -> List-of-words
; finds all rearrangements of word
(check-member-of (arrangements w1) (list (string->word "ab") (string->word "ba")))
                                 
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-words (first w)
            (arrangements (rest w)))]))



; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "abc") (list "a" "b" "c"))

(define (string->word s)
  (explode s))



; Word -> String
; converts w to a string
(check-expect (word->string (list "a" "b" "c")) "abc")

(define (word->string w)
  (implode w))



; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))



; String -> List-of-strings
; finds all words that the letters of some given word spell
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 












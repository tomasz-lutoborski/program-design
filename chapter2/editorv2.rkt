;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editorv2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 30) ; the height of the editor 
(define WIDTH 600) ; its width 
(define FONT-SIZE 24) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define SCENE (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 2 HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)


(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))
 
; data example 1: 
(make-editor all good)
 
; data example 2:
(make-editor lla good)


; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))


; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end (rest l) s))]))


; String String -> Editor
; creates editor from two string inputs

(check-expect (create-editor "ab" "cd") (make-editor (cons "b" (cons "a" '())) (cons "c" (cons "d" '()))))

(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))


; Lo1s -> Image
; converts Lo1s to text representation

(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))


; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor

(check-expect
 (editor-render (create-editor "ab" "cd"))
 (place-image/align
  (beside (text "ab" FONT-SIZE FONT-COLOR)
          CURSOR
          (text "cd" FONT-SIZE FONT-COLOR)) 1 1 "left" "top" SCENE))

(define (editor-render e)
  (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   SCENE))


; Editor -> Editor
; moves cursor left

(check-expect (editor-lft (create-editor "ab" "cd")) (create-editor "a" "bcd"))
(check-expect (editor-lft (create-editor "" "cd")) (create-editor "" "cd"))

(define (editor-lft editor)
  (cond
    [(empty? (editor-pre editor)) editor]
    [else (make-editor
           (rest (editor-pre editor))
           (cons (first (editor-pre editor)) (editor-post editor)))]))


; Editor -> Editor
; moves cursor right

(check-expect (editor-rgt (create-editor "ab" "cd")) (create-editor "abc" "d"))
(check-expect (editor-rgt (create-editor "ab" "")) (create-editor "ab" ""))

(define (editor-rgt editor)
  (cond
    [(empty? (editor-post editor)) editor]
    [else (make-editor
           (cons (first (editor-post editor)) (editor-pre editor))
           (rest (editor-post editor)))]))



; Editor -> Editor
; deletes 1String to the left

(check-expect (editor-del (create-editor "ab" "cd")) (create-editor "a" "cd"))
(check-expect (editor-del (create-editor "" "cd")) (create-editor "" "cd"))

(define (editor-del editor)
  (cond
    [(empty? (editor-pre editor)) editor]
    [else (make-editor
           (rest (editor-pre editor))
           (editor-post editor))]))


; insert the 1String k between pre and post

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "\b")
  (create-editor "c" "fgh"))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "left")
  (create-editor "cdef" "gh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))




; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))







    
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SCENE (empty-scene 200 20))
(define CURSOR (rectangle 2 20 "solid" "red"))

; String -> String
; removes last char of string
(check-expect (string-remove-last "hello") "hell")
(define (string-remove-last string)
  (if (= (string-length string) 0) string (substring string 0 (- (string-length string) 1))))

; Editor -> Bool
; Checks if text in editor doesn't overflow SCENE
(check-expect (correct-size? (make-editor "helllllllllllllllllllllo" "wooooooooooooooooooooooooooooooooooooooooooooooooooooorld")) #false)
(check-expect (correct-size? (make-editor "hello" "world")) #true)
(define (correct-size? editor)
  (if
   (or (> (image-width
           (beside/align "middle"
                         (text (editor-pre editor) 11 "black")
                         CURSOR
                         (text (editor-post editor) 11 "black")))
          (image-width SCENE))
       (< (image-width
           (beside/align "middle"
                         (text (editor-pre editor) 11 "black")
                         CURSOR
                         (text (editor-post editor) 11 "black")))
          1))
   #false #true))

; String -> String
; removes first char of string
(check-expect (string-remove-first "hello") "ello")
(define (string-remove-first string)
  (substring string 1 (string-length string)))


; String -> 1String
; gets last char of string
(check-expect (string-get-last "hello") "o")
(define (string-get-last string)
  (substring string (- (string-length string) 1) (string-length string)))

; String -> 1String
; gets first char of string
(check-expect (string-get-first "hello") "h")
(define (string-get-first string)
  (substring string 0 1))


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t


; Editor -> Image
; render image representing state of Editor
(define (render editor)
  (overlay/align "left" "center"
                 (beside/align "middle"
                               (text (editor-pre editor) 11 "black")
                               CURSOR
                               (text (editor-post editor) 11 "black"))
                 SCENE))

; Editor KeyEvent -> Editor
; given KeyEvent changes Editor accordingly
; given Char: appends char to Editor-pre
; given "\b": removes leftmost char in pre
; given "left"|"right": changes position of cursor
; given "\t"|"\r"|other: ignores

(check-expect (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world"))
(check-expect (edit (make-editor "hello" "world") "a") (make-editor "helloa" "world"))
(check-expect (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") " ") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld"))

(define (edit editor key)
  (cond
    [(= (string-length key) 1)
     (cond 
       [(and (string=? key "\b") (correct-size? (make-editor (string-remove-last (editor-pre editor)) (editor-post editor))))
        (make-editor (string-remove-last (editor-pre editor)) (editor-post editor))] 
       [(and (correct-size? (make-editor (string-append (editor-pre editor) key) (editor-post editor)))
             (not (or (string=? key "\t") (string=? key "\r"))))
        (make-editor
         (string-append (editor-pre editor) key)
         (editor-post editor))]
       [else editor])]
    [(string=? key "left")
     (make-editor
      (string-remove-last (editor-pre editor))
      (string-append (string-get-last (editor-pre editor)) (editor-post editor)))]
    [(string=? key "right")
     (make-editor
      (string-append (editor-pre editor) (string-get-first (editor-post editor)))
      (string-remove-first (editor-post editor)))]
    [else editor]
    )
  )

(define (run editor)
  (big-bang editor
    [to-draw render]
    [on-key edit]))
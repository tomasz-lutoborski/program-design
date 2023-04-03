#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;; An Attribute is a list of two items:
;;   (cons Symbol (cons String '()))

;; A Body is one of:
;; - '()
;; - (cons Xexpr Body)
;; - (cons [List-of Attribute] Body)

;; An Xexpr is a list (cons Symbol Body)

;; An XWord is '(word ((text String))).

;; An XEnum.v1 is one of:
;; – (cons 'ul [List-of XItem.v1])
;; – (cons 'ul (cons Attributes [List-of XItem.v1]))

;; An XItem.v1 is one of:
;; – (cons 'li (cons XWord '()))
;; – (cons 'li (cons Attributes (cons XWord '())))

;; An XItem.v2 is one of:
;; – (cons 'li (cons XWord '()))
;; – (cons 'li (cons [List-of Attribute] (list XWord)))
;; – (cons 'li (cons XEnum.v2 '()))
;; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
;;
;; An XEnum.v2 is one of:
;; – (cons 'ul [List-of XItem.v2])
;; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))


(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

(define word0 '(word ((text "Hello"))))
(define word1 '(word ((text "Bye"))))

(define item0 `(li ,word0))
(define item1 `(li ,word1))

(define enum0
  `(ul
    ,item0
    ,item1))

(define item2 `(,enum0 ,word0 ,word1))

(define enum1
  `(ul
    ,item0
    ,item1
    ,item2))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid "black") (text " " SIZE COLOR)))

;; Xexpr -> [List-of Attribute]
;; returns the attributes of the given Xexpr
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attr? loa-or-x)
             loa-or-x
             '()))])))


;; [List-of Attribute] or Xexpr -> Boolean
;; is the given item a list of attributes?
(check-expect (list-of-attr? '()) true)
(check-expect (list-of-attr? '(a)) false)
(check-expect (list-of-attr? '(a b)) false)
(check-expect (list-of-attr? '((a "b"))) true)
(check-expect (list-of-attr? '((a "b") (c "d"))) true)

(define (list-of-attr? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))


;; Xexpr -> Symbol
;; returns the name of the given Xexpr
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xexpr)
  (first xexpr))


;; Xexpr -> [List-of Xexpr]
;; returns the content of the given Xexpr
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attr? loa-or-x)
             (rest optional-loa+content)
             optional-loa+content))])))


;; [List-of Attribute] Symbol -> String or False
;; returns the attribute with the given name, or false if none
(check-expect (find-attr '() 'a) false)
(check-expect (find-attr '((a "b")) 'a) "b")
(check-expect (find-attr '((a "b") (c "d")) 'a) "b")
(check-expect (find-attr '((a "b") (c "d")) 'c) "d")

(define (find-attr loa name)
  (cond
    [(empty? loa) false]
    [else
     (local ((define attr (first loa)))
       (if (eq? (first attr) name)
           (second attr)
           (find-attr (rest loa) name)))]))

;; Xexpr -> Boolean
;; is the given Xexpr an XWord?
(check-expect (xword? e0) false)
(check-expect (xword? e1) false)
(check-expect (xword? word1) true)
(check-expect (xword? word0) true)

(define (xword? xexpr)
  (and (eq? (xexpr-name xexpr) 'word)
       (not (empty? (xexpr-attr xexpr)))
       (eq? (first (first (xexpr-attr xexpr))) 'text)))


;; XWord -> String
;; returns the text of the given XWord
(check-expect (xword-text word0) "Hello")
(check-expect (xword-text word1) "Bye")

(define (xword-text xword)
  (second (first (xexpr-attr xword))))

;; XItem.v1 -> Image
;; renders the given XItem.v1
(check-expect (render-item1 item0) (beside/align 'center BT (text "Hello" 12 'black)))
(check-expect (render-item1 item1) (beside/align 'center BT (text "Bye" 12 'black)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (xword-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))


;; XEnum.v1 -> Image
;; renders the given XEnum.v1
(check-expect (render-enum1 enum0)
              (above/align 'left
                           (render-item1 item0)
                           (render-item1 item1)))

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                          (render-item1 item)
                          so-far)))
    (foldr deal-with-one empty-image content)))


;; Image -> Image
;; marks item with bullet
(check-expect (bulletize (text "Hello" 12 'black))
              (beside/align 'center BT (text "Hello" 12 'black)))

(define (bulletize item)
  (beside/align 'center BT item))


;; XEnum.v2 -> Image
;; renders an XEnum.v2 as an image
(check-expect (render-enum enum0)
              (above/align 'left
                           (bulletize (text "Hello" 12 'black))
                           (bulletize (text "Bye" 12 'black))))
(check-expect (render-enum enum1)
              (above/align 'left
                           (render-item item0)
                           (render-item item1)
                           (render-item item0)))

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))


;; XItem.v2 -> Image
;; renders one XItem.v2 as an image
(check-expect (render-item item0)
              (bulletize (text "Hello" 12 'black)))
(check-expect (render-item item1) (bulletize (text "Bye" 12 'black)))

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(xword? content)
         (text (xword-text content) SIZE "black")]
        [else (render-enum content)]))))


;; XEnum.v2 String -> Number
;; counts the number of given string in the given XEnum.v2
(check-expect (count-enum enum0 "Hello") 1)
(check-expect (count-enum enum0 "Bye") 1)
(check-expect (count-enum enum0 "Hi") 0)
(check-expect (count-enum enum1 "Hello") 2)

(define (count-enum xe str)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Number -> Number
          (define (deal-with-one item so-far)
            (+ (count-item item str) so-far)))
    (foldr deal-with-one 0 content)))


;; XItem.v2 String -> Number
;; counts the number of given string in the given XItem.v2
(check-expect (count-item item0 "Hello") 1)
(check-expect (count-item item0 "Bye") 0)

(define (count-item an-item str)
  (local ((define content (first (xexpr-content an-item))))
    (cond
      [(xword? content)
       (if (eq? (xword-text content) str)
           1
           0)]
      [else (count-enum content str)])))


;; An FSM-State is a String that specifies a color.

;; A State-and-Trigger is a list of two items:
;;    (cons FSM-State (cons KeyEvent '()))

;; A Transition is a list of two items:
;;   (cons State-and-Trigger (cons FSM-State '()))

;; An FSM is a [List-of Transition]

;; An XMachine is a nested list of this shape:
;;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))

;; An X1T is a nested list of this shape:
;;   `(action ((state ,FSM-State) (next ,FSM-State)))


;; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

;; FSM FSM-State -> FSM-State
;; Matches the keys pressed by a player with the given FSM.
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay (text current 14 "black") (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions (cons current (cons key-event '()))))]))


;; [X Y] [List-of [List X Y]] X -> Y
;; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "blue") "not found")

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))


;; XMachine -> FSM-State
;; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))


;; XMachine -> FSM-State
;; extracts and translates the transition table from xm0
(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))


;; XMachine -> [List-of 1Transition]
;; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

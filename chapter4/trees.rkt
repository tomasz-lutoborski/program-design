#lang htdp/isl+



; A Child is a structure:
;   (make-child Child Child String N String)
(define-struct child [father mother name date eyes])

(define-struct no-parent [])

(define NP (make-no-parent))

; An FT is one of:
; – NP
; – (make-child FT FT String N String)


; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))
(define Helga (make-child Fred Eva "Helga" 1990 "pink"))


; FT -> Number
; count the number of children in an-ftree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

(define (count-persons ftree)
  (cond
    [(no-parent? ftree) 0]
    [else (+ 1 (count-persons (child-father ftree))
             (count-persons (child-mother ftree)))]))


; FT -> [String]
; produce a list of the eyes colors of all the children in an-ftree
(check-expect (all-eyes Carl) '("green"))
(check-expect (all-eyes Gustav) '("brown" "pink" "blue" "green" "green"))

(define (all-eyes ftree)
  (cond
    [(no-parent? ftree) '()]
    [else (append (list (child-eyes ftree))
            (all-eyes (child-father ftree))
            (all-eyes (child-mother ftree)))]))


; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))


; FT -> Boolean
; does an-ftree contain an ancestor with the blue eyes
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(check-expect (blue-eyed-ancestor? Carl) #false)

(define (blue-eyed-ancestor? ftree)
  (cond
    [(no-parent? ftree) #false]
    [else (or (blue-eyed? (child-father ftree))
              (blue-eyed? (child-mother ftree))
              (blue-eyed-ancestor? (child-father ftree))
              (blue-eyed-ancestor? (child-mother ftree)))]))


; Any -> Boolean
; is the given value a blue-eyed child
(check-expect (blue-eyed? Gustav) #false)
(check-expect (blue-eyed? Eva) #true)

(define (blue-eyed? ftree)
    (and (child? ftree)
         (string=? (child-eyes ftree) "blue")))


; An FF (short for family forest) is one of:
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))


; FF -> Boolean
; does the forest contain any child with "blue" eyes
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(define (blue-eyed-child-in-forest? a-forest)
  (cond
    [(empty? a-forest) #false]
    [else
     (or (blue-eyed-child? (first a-forest))
         (blue-eyed-child-in-forest? (rest a-forest)))]))


; FF -> Number
; calculates average age of all the children in the forest
(check-expect (average-age ff1 2023) 97)
(check-expect (average-age ff2 2023) 77.25)

(define (average-age a-forest current-year)
  (local (
          (define (sum-age ftree)
            (cond
              [(no-parent? ftree) 0]
              [else (+ (- current-year (child-date ftree))
                       (sum-age (child-father ftree))
                       (sum-age (child-mother ftree)))]))
          (define (count-children ftree)
            (cond
              [(no-parent? ftree) 0]
              [else (+ 1 (count-children (child-father ftree))
                        (count-children (child-mother ftree)))]))
          (define (sum-all-ages a-forest)
            (cond
              [(empty? a-forest) 0]
              [else (+ (sum-age (first a-forest))
                       (sum-all-ages (rest a-forest)))]))
          (define (count-all-children a-forest)
            (cond
              [(empty? a-forest) 0]
              [else (+ (count-children (first a-forest))
                       (count-all-children (rest a-forest)))])))
    (/ (sum-all-ages a-forest) (count-all-children a-forest))))

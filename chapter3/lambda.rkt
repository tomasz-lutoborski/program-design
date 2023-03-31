#lang htdp/isl+

(define mystery (lambda (x y)
                  (+ x
                     (local ((define z (* y y)))
                       (+ (* 3 z) (/ 1 x))))))


; [List-of Posn] -> [List-of Posn]
(check-expect (add-3-to-all (list (make-posn 1 2) (make-posn 3 4)))
              (list (make-posn 4 2) (make-posn 6 4)))

(define (add-3-to-all lop)
  (map (lambda (p)
         (make-posn (+ (posn-x p) 3) (posn-y p)))
       lop))

; [List-of Euro] -> [List-of Dollars]
; Converts a list of Euros to a list of Dollars
(check-expect (convert-to-dollars (list 1 2 3))
              (list 1.2 2.4 3.6))

(define (convert-to-dollars le)
  (map (lambda (e)
         (* e 1.2))
       le))

; [List-of Posn] -> [List-of [List-of Number]]
; Returns a list of lists of numbers, where each list of numbers
; is the x and y coordinates of the corresponding position in the
; list of positions
(check-expect (posn-to-coords (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))

(define (posn-to-coords lop)
  (map (lambda (p)
         (list (posn-x p) (posn-y p)))
       lop))

; Inventory is a structure with the following fields:
; - name: String
; - description: String
; - acquisition-price: Number
; - resale-price: Number
(define-struct inventory (name description acquisition-price resale-price))

(define item1 (make-inventory "item1" "description1" 10 20))
(define item2 (make-inventory "item2" "description2" 20 40))
(define item3 (make-inventory "item3" "description3" 30 60))

; [List-of Inventory] -> [List-of Inventory]
; Sorts a list of inventory items by their acquisition price
(check-expect (sort-by-price-diff (list item1 item2 item3))
              (list item1 item2 item3))
(check-expect (sort-by-price-diff (list item2 item1 item3))
              (list item1 item2 item3))

(define (sort-by-price-diff l)
  (local
      ((define (price-diff i)
         (- (inventory-resale-price i) (inventory-acquisition-price i))))
    (sort l (lambda (i1 i2) (< (price-diff i1) (price-diff i2))))))


; Number -> [List-of Number]
; Creates a list of numbers from 0 to n - 1
(check-expect (create-list 0) (list))
(check-expect (create-list 1) (list 0))
(check-expect (create-list 4) (list 0 1 2 3))

(define (create-list n)
  (build-list n (lambda (x) x)))

; Number -> [List-of Number]
; Creates a list of numbers from 1 to n
(check-expect (create-list-from-1 0) (list))
(check-expect (create-list-from-1 1) (list 1))
(check-expect (create-list-from-1 4) (list 1 2 3 4))

(define (create-list-from-1 n)
  (build-list n (lambda (x) (+ x 1))))

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(check-expect ((sorted <) '()) #true)
(check-expect ((sorted <) (list 1 2 3)) #true)
(check-expect ((sorted <) (list 1 3 2)) #false)
(check-expect ((sorted <=) (list 1 2 2)) #true)

(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

; [X X -> Boolean] -> [List-of X] -> Boolean
; checks if the given list l0 is sorted according to cmp
(check-expect (sorted? < '()) #true)
(check-expect (sorted? < (list 1 2 3)) #true)
(check-expect (sorted? < (list 1 3 2)) #false)

(define (sorted? cmp l)
  ((sorted cmp) l))

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
(define (sort-cmp/worse l)
  (local ((define sorted (sort-cmp l <)))
    (cons (- (first sorted) 1) sorted)))
; [List-of Number] [Number Number -> Boolean]
; -> [List-of Number]
; produces a version of alon0, sorted according to cmp

(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))

          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))



; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-expect (index 10 '()) #false)
(check-expect (index 10 '(1 2 3)) #false)
(check-expect (index 10 '(10 20 30)) 0)
(check-expect (index 10 '(20 10 10)) 1)
(check-expect (index 10 '(20 30 10)) 2)
(check-satisfied (index 10 '()) (is-index? 10 '()))
(check-satisfied (index 10 '(1 2 3)) (is-index? 10 '(1 2 3)))
(check-satisfied (index 10 '(10 20 30)) (is-index? 10 '(10 20 30)))
(check-satisfied (index 10 '(20 30 10)) (is-index? 10 '(20 30 10)))

(define (index x l)
    (cond
        [(empty? l) #false]
        [else (if (equal? (first l) x)
                0
                (local ((define i (index x (rest l))))
                    (if (boolean? i) i (+ i 1))))]))


; [X] [List-of X] -> [[Maybe N] -> Boolean]
; specification function for index
(define (is-index? x l)
    (lambda (i)
      (local (;; X [List-of X] -> Boolean
              (define (not-found? x l)
                (and (false? i) (not (member? x l))))

              ;; N [List-of X] -> Boolean
              (define (is-valid? i l)
                (and
                 (< i (length l))
                 (eq? (list-ref l i) x)
                 (not (member? x (build-list i (lambda (j) (list-ref l j))))))))
        (or (not-found? x l) (is-valid? i l)))))

; A Shape is a function:
;   [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p)
; produces #true if p is in s, #false otherwise
 

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))


; Number Number -> Shape
(check-expect (inside? (mk-point 3 4) (make-posn 3 4))
              #true)
(check-expect (inside? (mk-point 3 4) (make-posn 3 0))
              #false)

(define (mk-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))

(define a-sample-shape (mk-point 3 4))



(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))


(define (distance-between x1 y1 p)
  (sqrt (+ (sqr (- x1 (posn-x p)))
           (sqr (- y1 (posn-y p))))))


; Number Number Number Number -> Shape
; represents a width by height rectangle whose
; upper-left corner is located at (ul-x, ul-y)
(check-expect (inside? (mk-rect 0 0 10 3)
                       (make-posn 0 0))
              #true)
(check-expect (inside? (mk-rect 2 3 10 3)
                       (make-posn 4 5))
              #true)
(check-expect (inside? (mk-rect 2 3 10 3)
                       (make-posn 4 2))
              #false)

(define (mk-rect ul-x ul-y width height)
  (lambda (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))

; Shape Shape -> Shape
; combines two shapes into one
(check-expect (inside? union1 (make-posn 0 0)) #true)
(check-expect (inside? union1 (make-posn 0 9)) #false)
(check-expect (inside? union1 (make-posn -1 3)) #true)

(define (mk-combination s1 s2)
  ; Posn -> Boolean
  (lambda (p)
    (or (inside? s1 p) (inside? s2 p))))

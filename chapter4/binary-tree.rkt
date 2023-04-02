#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)
(define bt1 (make-node 1 'a NONE NONE))
(define bt2 (make-node 2 'b NONE NONE))
(define bt3 (make-node 3 'c bt1 bt2))
(define bt4 (make-node 4 'd bt3 NONE))


; BT Number -> Symbol
; Returns the name of the node with the given ssn
(check-expect (search-bt bt1 1) 'a)
(check-expect (search-bt bt4 2) 'b)

(define (search-bt bt ssn)
  (cond [(no-info? bt) NONE]
        [(= ssn (node-ssn bt)) (node-name bt)]
        [else (cond
                [(contains-bt? (node-left bt) ssn) (search-bt (node-left bt) ssn)]
                [(contains-bt? (node-right bt) ssn) (search-bt (node-right bt) ssn)]
                [else NONE])]))


; BT Number -> Boolean
; Returns true if the given ssn is in the given BT
(check-expect (contains-bt? bt1 1) true)
(check-expect (contains-bt? bt4 2) true)
(check-expect (contains-bt? bt4 5) false)

(define (contains-bt? bt ssn)
  (cond [(no-info? bt) false]
        [(= ssn (node-ssn bt)) true]
        [else (or
                (contains-bt? (node-left bt) ssn)
                (contains-bt? (node-right bt) ssn))]))

; BT -> [List-of Number]
; consumes a BT and produces a list of all the ssn's in the BT in order
(check-expect (ssn-list bt1) (list 1))
(check-expect (ssn-list bt4) (list 1 3 2 4))

(define (ssn-list bt)
  (cond [(no-info? bt) empty]
        [else (append (ssn-list (node-left bt))
                      (cons (node-ssn bt)
                            (ssn-list (node-right bt))))]))


; BT Number Symbol -> BT
; creates binary tree with node inserted in correct place
(check-expect (create-bt bt1 2 'b) (make-node 1 'a NONE (make-node 2 'b NONE NONE)))
(check-expect (create-bt NONE 1 'z)
              (make-node 1 'z NONE NONE))

(define (create-bt bt ssn name)
  (cond
    [(no-info? bt) (make-node ssn name NONE NONE)]
    [else (make-node
           (node-ssn bt)
           (node-name bt)
           (if (< ssn (node-ssn bt))
               (create-bt (node-left bt) ssn name)
               (node-left bt))
           (if (> ssn (node-ssn bt))
               (create-bt (node-right bt) ssn name)
               (node-right bt)))]))

; BT [List-of [List Number Symbol]] -> BT
; consumes a BT and a list of lists of ssn and name
; and produces a BT with all the nodes in the list
(check-expect (create-bt-from-list bt1 (list (list 2 'b) (list 3 'c)))
              (make-node 1 'a NONE (make-node 2 'b NONE (make-node 3 'c NONE NONE))))
(check-expect (create-bt-from-list NONE (list (list 1 'a))) (make-node 1 'a NONE NONE))
(check-expect (create-bt-from-list NONE empty) NONE)

(define (create-bt-from-list bt list)
  (cond [(empty? list) bt]
        [else (create-bt-from-list (create-bt bt (first (first list)) (second (first list)))
                                   (rest list))]))



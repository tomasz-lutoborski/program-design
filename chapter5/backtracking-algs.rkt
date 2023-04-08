#lang htdp/isl+

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define cyclic-graph
  '((A B E)
    (B E F)
    (C D B)
    (D)
    (E C F)
    (F D G)
    (G)))
; A Node is a Symbol.


; Node Graph -> [List-of Node]
; Returns a list of neighbours of given node.
(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'B sample-graph) '(E F))
(check-expect (neighbors 'C sample-graph) '(D))
(check-expect (neighbors 'D sample-graph) '())

(define (neighbors node graph)
  (local ((define found (assoc node graph)))
    (if (null? found)
        '()
        (rest found))))


; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first
; Node on the list to the last one.

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
(check-expect (find-path 'A 'G sample-graph)
              '(A B E F G))

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))


; Graph -> Boolean
; determines whether there is a path between any two nodes
(check-expect (check-on-all-nodes sample-graph) #false)

(define (check-on-all-nodes G)
  (local ((define nodes (map first G)))
    (andmap (lambda (n)
              (andmap (lambda (m)
                        (not (boolean? (find-path n m G))))
                      nodes))
            nodes)))

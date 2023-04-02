#lang htdp/isl+

; An S-expr is one of:
; – Atom
; – SL

; An SL is one of:
; – '()
; – (cons S-expr SL)

; An Atom is one of:
; – Number
; – String
; – Symbol

; Any -> Boolean
; Returns true if the given value is an Atom
(check-expect (atom? 1) #t)
(check-expect (atom? "hello") #t)
(check-expect (atom? 'hello) #t)
(check-expect (atom? (cons 1 '())) #f)

(define (atom? x)
  (cond
    [(number? x) #t]
    [(string? x) #t]
    [(symbol? x) #t]
    [else #f]))



; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 1 'hello) 0)
(check-expect (count "hello" 'hello) 0)
(check-expect (count 'hello 'hello) 1)
(check-expect (count '() 'hello) 0)
(check-expect (count (cons 1 '()) 'hello) 0)
(check-expect (count (cons 'hello '()) 'hello) 1)

(define (count sexp sy)
  (local (
          (define (count-sl sl sy)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl) sy))]))
          (define (count-atom at sy)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp sy)]
      [else (count-sl sexp sy)])))


; S-expr -> Number
; calculates depth of sexp
(check-expect (depth 1) 1)
(check-expect (depth "hello") 1)
(check-expect (depth 'hello) 1)
(check-expect (depth (cons 'hello (cons 'world '()))) 2)

(define (depth sexp)
  (local (
          (define (depth-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (depth (first sl)) (depth-sl (rest sl)))]))
          (define (depth-atom at)
            1))
    (cond
      [(atom? sexp) (depth-atom sexp)]
      [else (depth-sl sexp)])))


; S-expr X Y -> S-expr
; replaces all occurrences of old with new in sexp
(check-expect (replace 1 1 2) 2)
(check-expect (replace "hello" "hello" "world") "world")
(check-expect (replace (cons 1 (cons 2 '())) 1 3) (cons 3 (cons 2 '())))

(define (replace sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [(empty? sexp) '()]
    [(equal? (first sexp) old) (cons new (replace (rest sexp) old new))]
    [else (cons (first sexp) (replace (rest sexp) old new))]))


; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(((world) bye) bye) 'hello '42)
              '(((world) bye) bye))
(check-expect (substitute '(((world) bye) bye) 'bye 'hello)
              '(((world) hello) hello))
(check-expect (substitute '(((world) bye) bye) 'hello 'hello)
              '(((world) bye) bye))
(check-expect (substitute '(((world) bye) bye) 'bye 'bye)
              '(((world) bye) bye))
(check-expect (substitute '() 'hello 'hello)
              '())

(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else (map (lambda (x) (substitute x old new)) sexp)]))

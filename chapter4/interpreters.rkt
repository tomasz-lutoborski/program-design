#lang htdp/isl+

(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-expr BSL-expr)
;; (make-add expr1 expr2) represents
;; a BSL expression for addition of expr1 and expr2.

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-expr BSL-expr)
;; (make-mul expr1 expr2) represents
;; a BSL expression for multiplication of expr1 and expr2.

;; A BSL-expr is one of:
;; - Number
;; - Symbol
;; - Add
;; - Mul
;; - Fun

(define-struct fun [name arg])
;; A Fun is a structure:
;;  (make-fun Symbol BSL-expr)
;; (make-fun name expr) represents
;; a BSL expression for a function with name name and argument arg.

(define-struct or-expr [left right])
;; An Or is a structure:
;;   (make-or BSL-bool-expr BSL-bool-expr)
;; (make-or expr1 expr2) represents
;; a BSL expression for the logical or of expr1 and expr2.

(define-struct and-expr [left right])
;; An And is a structure:
;;  (make-and BSL-bool-expr BSL-bool-expr)
;;  (make-and expr1 expr2) represents
;; a BSL expression for the logical and of expr1 and expr2.

(define-struct not-expr [expr])
;; A Not is a structure:
;; (make-not BSL-bool-expr)
;; (make-not expr) represents
;; a BSL expression for the logical not of expr.

;; A BSL-bool-expr is one of:
;; - Boolean
;; - Or
;; - And
;; - Not

;; An S-expr is one of:
;; – Atom
;; – SL

;; An SL is one of:
;; – '()
;; – (cons S-expr SL)

;; An Atom is one of:
;; – Number
;; – String
;; – Symbol

;; A BSL-value is a Number

;; An AL (short for association list) is [List-of Association].
;; An Association is a list of two items:
;;   (cons Symbol (cons Number '())).
(define a-1 (cons 'x (cons 3 '())))
(define a-2 (cons 'y (cons 5 '())))
(define al (list a-1 a-2))



(define WRONG "Invalid expression.")

(define NOT-NUMERIC "Not numeric.")

(define WRONG-FUNC-NAME "Wrong function name.")

(define NOT-FOUND "Fundef not found.")

;; BSL-expr -> Number
;; Evaluates a BSL expression to a number.
(check-expect (eval-expr (make-add 3 3)) 6)
(check-expect (eval-expr (make-mul 3 3)) 9)
(check-expect (eval-expr (make-mul (make-add 3 3) 3)) 18)
(check-expect (eval-expr (make-mul (make-add 3 3) (make-add 3 3))) 36)

(define (eval-expr expr)
  (cond
    [(number? expr) expr]
    [(add? expr) (+ (eval-expr (add-left expr)) (eval-expr (add-right expr)))]
    [(mul? expr) (* (eval-expr (mul-left expr)) (eval-expr (mul-right expr)))]))


;; BSL-bool-expr -> Boolean
;; Evaluates a BSL boolean expression to a boolean.
(check-expect (eval-bool-expr #t) #t)
(check-expect (eval-bool-expr #f) #f)
(check-expect (eval-bool-expr (make-or-expr #t #f)) #t)
(check-expect (eval-bool-expr (make-or-expr #f #f)) #f)
(check-expect (eval-bool-expr (make-and-expr #t #f)) #f)
(check-expect (eval-bool-expr (make-and-expr #t #t)) #t)
(check-expect (eval-bool-expr (make-not-expr #t)) #f)

(define (eval-bool-expr expr)
  (cond
    [(boolean? expr) expr]
    [(or-expr? expr) (or (eval-bool-expr (or-expr-left expr)) (eval-bool-expr (or-expr-right expr)))]
    [(and-expr? expr) (and (eval-bool-expr (and-expr-left expr)) (eval-bool-expr (and-expr-right expr)))]
    [(not-expr? expr) (not (eval-bool-expr (not-expr-expr expr)))]))


;; S-expr -> BSL-expr
(check-expect (parse 3) 3)
(check-error (parse 'a) WRONG)
(check-expect (parse (list '+ 3 3)) (make-add 3 3))
(check-expect (parse (list '* 3 3)) (make-mul 3 3))
(check-expect (parse (list '* (list '+ 3 3) 3)) (make-mul (make-add 3 3) 3))
(check-error (parse (list '+ 3)) WRONG)
(check-error (parse (list '+ 3 3 3)) WRONG)

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))


;; SL -> BSL-expr
(check-expect (parse (list '+ 3 3)) (make-add 3 3))
(check-expect (parse (list '* 3 3)) (make-mul 3 3))
(check-expect (parse (list '* (list '+ 3 3) 3)) (make-mul (make-add 3 3) 3))

(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))


;; Atom -> BSL-expr
(check-expect (parse-atom 3) 3)
(check-error (parse-atom 'a) WRONG)
(check-error (parse-atom "a") WRONG)

(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))


;; SL -> Boolean
(check-expect (consists-of-3 (list 1 2 3)) #t)
(check-expect (consists-of-3 (list 1 2 3 4)) #f)
(check-expect (consists-of-3 (list 1 2)) #f)
(check-expect (consists-of-3 (list 1 2 3 (list 4 5))) #f)
(check-expect (consists-of-3 (list 1 2 (list 4 5))) #t)

(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

;; Any -> Boolean
(define (atom? s)
  (or (number? s) (string? s) (symbol? s)))

;; Sexp -> BSL-value
(check-expect (interpreter-expr '(+ 3 3)) 6)
(check-expect (interpreter-expr '(* 3 3)) 9)
(check-expect (interpreter-expr '(* (+ 3 3) 3)) 18)
(check-expect (interpreter-expr '(* (+ 3 3) (+ 3 3))) 36)
(check-error (interpreter-expr '(* (+ 3 3) (+ 3 3) (+ 3 3))) WRONG)
(check-error (interpreter-expr '(+ 3 3 3)) WRONG)
(check-error (interpreter-expr '(+ 3)) WRONG)

(define (interpreter-expr s)
  (cond
    [(atom? s) (error WRONG)]
    [else (eval-expr (parse s))]))


;; BSL-expr Symbol Number -> BSL-expr
;; Replaces all occurrences of the symbol in the expression with the number.
(check-expect (replace (make-add 3 3) 'a 3) (make-add 3 3))
(check-expect (replace (make-add 'a 3) 'a 3) (make-add 3 3))
(check-expect (replace (make-add 3 'a) 'a 3) (make-add 3 3))
(check-expect (replace (make-add 'a 'a) 'a 3) (make-add 3 3))
(check-expect (replace 'a 'a 3) 3)

(define (replace expr symbol number)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (if (symbol=? expr symbol) number expr)]
    [(add? expr) (make-add (replace (add-left expr) symbol number) (replace (add-right expr) symbol number))]
    [(mul? expr) (make-mul (replace (mul-left expr) symbol number) (replace (mul-right expr) symbol number))]))


;; BSL-expr -> Bool
;; Checks if the expression is numeric.
(check-expect (numeric? 3) #t)
(check-expect (numeric? 'a) #f)
(check-expect (numeric? (make-add 3 3)) #t)
(check-expect (numeric? (make-add 3 'a)) #f)


;; BSL-var-expr -> BSL-value
(check-expect (eval-variable 100) 100)
(check-error (eval-variable 'x) NOT-NUMERIC)
(check-expect (eval-variable (make-add 5 3)) 8)
(check-error (eval-variable (make-add 'x 3)) NOT-NUMERIC)
(check-expect (eval-variable (make-mul 1/2 (make-mul 5 4))) 10)
(check-error (eval-variable (make-mul 1/2 (make-mul 'x 3))) NOT-NUMERIC)
(check-expect (eval-variable (make-add (make-mul 1 2) (make-add 10 2))) 14)
(check-error (eval-variable (make-add (make-mul 'x 'x) (make-add 'y 'y))) NOT-NUMERIC)
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error NOT-NUMERIC)))


;; BSL-expr -> Bool
;; Checks if the given BSL-expr is numeric.
(define (numeric? expr)
  (cond
    [(number? expr) #t]
    [(add? expr) (and (numeric? (add-left expr)) (numeric? (add-right expr)))]
    [(mul? expr) (and (numeric? (mul-left expr)) (numeric? (mul-right expr)))]
    [else #f]))

;; BSL-expr -> BSL-value
;; Computes value of the given BSL-expr.
(check-expect (eval-expression 10) 10)
(check-expect (eval-expression (make-add 10 -10)) 0)
(check-expect (eval-expression (make-add (make-mul 20 3) 33)) 93)
(check-expect (eval-expression
               (make-mul (make-add 20 (make-add 10 10)) (make-mul 3 2))) 240)
(define (eval-expression ex)
  (cond
    [(number? ex) ex]
    [(add? ex)
     (+ (eval-expression (add-left ex))
        (eval-expression (add-right ex)))]
    [(mul? ex)
     (* (eval-expression (mul-left ex))
        (eval-expression (mul-right ex)))]))


;; BSL-var-expr AL -> BSL-value
;; Determines value of ex.
;; If ex is not numeric, produces NOT-NUMERIC error.
(check-expect (eval-variable* 100 '()) 100)
(check-expect (eval-variable* 100 al) 100)
(check-error (eval-variable* 'x '()) NOT-NUMERIC)
(check-expect (eval-variable* 'x al) 3)
(check-error (eval-variable* 'z al) NOT-NUMERIC)
(check-expect (eval-variable* (make-add 5 3) '()) 8)
(check-expect (eval-variable* (make-add 5 3) al) 8)
(check-expect (eval-variable* (make-add 'x 3) al) 6)
(check-error (eval-variable* (make-add 'z 3) al) NOT-NUMERIC)
(check-expect (eval-variable* (make-mul 1/2 (make-mul 'y 4)) al) 10)
(check-error (eval-variable* (make-mul 1/2 (make-mul 'z 3)) al) NOT-NUMERIC)
(check-expect (eval-variable* (make-add (make-mul 'x 'x) (make-add 'y 'y)) al) 19)
(check-error (eval-variable* (make-add (make-mul 'x 'x) (make-add 'z 'z)) al)
             NOT-NUMERIC)
(define (eval-variable* ex l)
  (cond
    [(empty? l) (if (numeric? ex) (eval-expression ex) (error NOT-NUMERIC))]
    [else (eval-variable*
           (replace ex (first (first l)) (second (first l)))
           (rest l))]))


(define fun-k
  (make-fun 'k (make-add 1 1)))
(define mul-5
  (make-mul 5 fun-k))

;; BSL-expr Symbol Symbol BSL-expr -> Number
;; Determines the value of ex.
(check-expect (eval-definition1 10 'f 'x (make-add 'x 2)) 10)
(check-expect (eval-definition1 (make-add 1 2) 'f 'x  (make-add 'x 2)) 3)
(check-expect (eval-definition1 fun-k 'k 'x (make-add 'x 2))
              (+ (+ 1 1) 2))
(check-expect (eval-definition1 mul-5 'k 'x (make-add 'x 2))
              (* 5 (+ (+ 1 1) 2)))
(check-expect (eval-definition1
               (make-mul 5 (make-fun 'g (make-add 1 (make-fun 'g (make-mul 2 3)))))
               'g 'x
               (make-add 'x 2))
              (* 5 (+ (+ 1 (+ (* 2 3) 2)) 2)))
(check-error (eval-definition1 fun-k 'f 'x (make-mul 'x 10)) WRONG-FUNC-NAME)
(check-error (eval-definition1
              (make-fun 'k (make-add 'x 1))
              'k 'z
              (make-mul 'z 10))
             NOT-NUMERIC)
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error NOT-NUMERIC)]
    [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                  (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                  (eval-definition1 (mul-right ex) f x b))]
    [(fun? ex)
     (if (not (symbol=? (fun-name ex) f))
         (error WRONG-FUNC-NAME)
         (local ((define value (eval-definition1 (fun-arg ex) f x b))
                 (define plugd (subst b x value)))
           (eval-definition1 plugd f x b)))]))


;; BSL-fun-expr Symbol Number -> BSL-fun-expr
;; Produces a BSL-fun-expr like ex
;; with the occurrences of x replaced by v.
(check-expect (subst 100 'x 3) 100)
(check-expect (subst 'x 'x 10) 10)
(check-expect (subst 'x 'y 10) 'x)
(check-expect (subst (make-add 'x 3) 'x 20) (make-add 20 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 10)
              (make-mul 1/2 (make-mul 10 3)))
(check-expect (subst (make-add (make-mul 'x 'x) (make-mul 'y 'y)) 'y 5)
              (make-add (make-mul 'x 'x) (make-mul 5 5)))
(check-expect (subst (make-fun 'f (make-add 2 'y)) 'y 3)
              (make-fun 'f (make-add 2 3)))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun
                (fun-name ex)
                (subst (fun-arg ex) x v))]))


(define-struct fundef [name param body])
;; A Fundef is a structure:
;;   (make-fundef Symbol Symbol BSL-expr)
;; (make-fundef f p x) represents a function definition
;; with the function's name f,
;; the function's parameter p,
;; and the function's body x.

(define (f x) (+ 3 x))
; ==
(define fundef-f
  (make-fundef 'f 'x (make-add 3 'x)))

(define (g y) (f (* 2 y)))
; ==
(define fundef-g
  (make-fundef 'g 'y (make-fun 'f (make-mul 2 'y))))

(define (h v) (+ (f v) (g v)))
; ==
(define fundef-h
  (make-fundef 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

(define fundef-list (list fundef-f fundef-g fundef-h))

;; [List-of Fundef] Symbol -> Fundef
;; Retrieves the definition of f in l.
;; Signals an error if there is none.
(check-expect (lookup-def fundef-list 'f) fundef-f)
(check-expect (lookup-def fundef-list 'h) fundef-h)
(check-error (lookup-def '() 'f) NOT-FOUND)
(define (lookup-def l f)
  (cond
    [(empty? l) (error NOT-FOUND)]
    [else (if (symbol=? f (fundef-name (first l)))
              (first l)
              (lookup-def (rest l) f))]))



;; BSL-fun-expr Symbol Number -> BSL-fun-expr
;; Produces a BSL-fun-expr like ex
;; with the occurrences of x replaced by v
;; excluding the Fun structures.
(check-expect (subst-fun 100 'x 3) 100)
(check-expect (subst-fun 'x 'x 10) 10)
(check-expect (subst-fun 'x 'y 10) 'x)
(check-expect (subst-fun (make-add 'x 3) 'x 20) (make-add 20 3))
(check-expect (subst-fun (make-mul 2 (make-mul 'x 3)) 'x 10)
              (make-mul 2 (make-mul 10 3)))

(define (subst-fun ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex)
     (make-add (subst-fun (add-left ex) x v)
               (subst-fun (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst-fun (mul-left ex) x v)
               (subst-fun (mul-right ex) x v))]
    [(fun? ex) (make-fun
                (fun-name ex)
                (subst-fun (fun-arg ex) x v))]))


;; BSL-fun-expr [List-of Fundef] -> Number
;; Determines the value of ex.
(check-expect (eval-function* (make-fun 'f 10) fundef-list) 13)
(define (eval-function* ex l)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error NOT-NUMERIC)]
    [(add? ex) (+ (eval-function* (add-left ex) l)
                  (eval-function* (add-right ex) l))]
    [(mul? ex) (* (eval-function* (mul-left ex) l)
                  (eval-function* (mul-right ex) l))]
    [(fun? ex)
     (local ((define found-fundef (lookup-def l (fun-name ex)))
             (define param (fundef-param found-fundef))
             (define body (fundef-body found-fundef))
             (define value (eval-function* (fun-arg ex) l))
             (define plugd (subst-fun body param value)))
       (eval-function* plugd l))]))

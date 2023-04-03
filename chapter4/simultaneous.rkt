#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)
; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
                (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))


; [List-of Number] [List-of Symbol] -> [List-of [list Number Symbol]]
; takes a list of numbers and a list of symbols and returns a list of
; lists of the form (number symbol)
(check-expect (cross '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(check-expect (cross '(1 2 3) '()) '())
(check-expect (cross '() '(a b c)) '())

(define (cross nums syms)
  (cond
    [(empty? nums) '()]
    [(empty? syms) '()]
    [else
     (cons (list (first nums) (first syms))
           (cross (rest nums) (rest syms)))]))

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on
; hours and wages/h
; assume the two lists are of equal length
(check-expect (wages* '() '()) '())
(check-expect (wages* (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages* '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages* hours wages)
  (cond
    [(empty? hours) '()]
    [(empty? wages) '()]
    [else
     (cons (* (first hours) (first wages))
           (wages* (rest hours) (rest wages)))]))


; Employee is a structure with the following fields:
; - name: String
; - pay-rate: Number
; - social-security: Number
(define-struct employee (name pay-rate social-security))

; Work record is a structure with the following fields:
; - employee: String
; - hours: Number
(define-struct work-record (employee hours))

; Employee-Pay is a structure with the following fields:
; - employee: String
; - pay: Number
(define-struct employee-pay (employee pay))

; [List-of Employee] [List-of Work-Record] -> [List-of Employee-Pay]
; takes a list of employees and a list of work records and returns a
; list of employee-pays
(check-expect (payroll '() '()) '())
(check-expect (payroll (list (make-employee "Bob" 5.65 123456789))
                       (list (make-work-record "Bob" 40.0)))
              (list (make-employee-pay "Bob" 226.0)))
(check-expect (payroll (list (make-employee "Bob" 5.65 123456789)
                             (make-employee "Sally" 8.75 987654321))
                       (list (make-work-record "Bob" 40.0)
                             (make-work-record "Sally" 30.0)))
              (list (make-employee-pay "Bob" 226.0)
                    (make-employee-pay "Sally" 262.5)))

(define (payroll employees work-records)
  (cond
    [(empty? employees) '()]
    [(empty? work-records) '()]
    [else
     (cons (make-employee-pay (employee-name (first employees))
                         (* (employee-pay-rate (first employees)) (work-record-hours (first work-records))))
           (payroll (rest employees) (rest work-records)))]))


; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l;
; signals an error if there is no such symbol
(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list-pick: list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list-pick: list too short")

(define (list-pick l n)
  (cond
    [(empty? l) (error 'list-pick "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))



(define-struct branch [left right])

; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)

; A Direction is one of:
; – 'left
; – 'right

; A list of Directions is also called a path.


; [TOS] [List-of Direction] -> TOS
; returns the TOS at the end of the path
(check-expect (follow-path (make-branch 'a 'b) '()) (make-branch 'a 'b))
(check-expect (follow-path (make-branch 'a 'b) '(left)) 'a)
(check-expect (follow-path (make-branch 'a 'b) '(right)) 'b)
(check-expect (follow-path (make-branch (make-branch 'a 'b) 'c) '(left left)) 'a)
(check-expect (follow-path (make-branch (make-branch 'a 'b) 'c) '(left right)) 'b)
(check-expect (follow-path (make-branch (make-branch 'a 'b) 'c) '(right)) 'c)

(define (follow-path tos path)
  (cond
    [(empty? path) tos]
    [(eq? (first path) 'left) (follow-path (branch-left tos) (rest path))]
    [(eq? (first path) 'right) (follow-path (branch-right tos) (rest path))]))


; Set is one of the following:
; – empty
; – (cons Number Set)

; Set Set -> Set
; returns the union of the two sets
(check-expect (union empty empty) empty)
(check-expect (union (cons 1 empty) empty) (cons 1 empty))
(check-expect (union empty (cons 1 empty)) (cons 1 empty))
(check-expect (union (cons 1 empty) (cons 2 empty)) (cons 1 (cons 2 empty)))
(check-expect (union (cons 1 empty) (cons 1 empty)) (cons 1 empty))

(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [(empty? s2) s1]
    [(member? (first s1) s2) (union (rest s1) s2)]
    [else (cons (first s1) (union (rest s1) s2))]))


; Set Set -> Set
; returns the intersection of the two sets
(check-expect (intersection empty empty) empty)
(check-expect (intersection (cons 1 empty) empty) empty)
(check-expect (intersection empty (cons 1 empty)) empty)
(check-expect (intersection (cons 1 empty) (cons 2 empty)) empty)
(check-expect (intersection (cons 1 empty) (cons 1 empty)) (cons 1 empty))

(define (intersection s1 s2)
  (cond
    [(or (empty? s1) (empty? s2)) empty]
    [(member? (first s1) s2) (cons (first s1) (intersection (rest s1) s2))]
    [else (intersection (rest s1) s2)]))


; [List-of Number] [List-of Number] -> [List-of Number]
; merges two sorted lists into a single sorted list
(check-expect (merge '() '()) '())
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '() '(1)) '(1))
(check-expect (merge '(1) '(2)) '(1 2))
(check-expect (merge '(2) '(1)) '(1 2))
(check-expect (merge '(1 3) '(2)) '(1 2 3))
(check-expect (merge '(1 3) '(2 4)) '(1 2 3 4))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(< (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2))]
    [else (cons (first l2) (merge l1 (rest l2)))]))


; [List-of Number] Number -> [List-of Number]
; takes n elements from the front of l
(check-expect (take '() 0) '())
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(check-expect (take '(1 2 3) 2) '(1 2))

(define (take l n)
  (cond
    [(or (empty? l) (= n 0)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))


(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))


; String HM-Word 1String -> HM-Word
; checks if the letter is in the word, if so, replaces the "_" with the letter
(check-expect (compare-word (explode "hello") '("_" "_" "_" "_" "_") "h") '("h" "_" "_" "_" "_"))
(check-expect (compare-word (explode "hello") '("h" "_" "_" "_" "_") "e") '("h" "e" "_" "_" "_"))
(check-expect (compare-word (explode "hello") '("h" "e" "_" "_" "_") "l") '("h" "e" "l" "l" "_"))
(check-expect (compare-word (explode "hello") '("h" "e" "l" "l" "_") "o") '("h" "e" "l" "l" "o"))
(check-expect (compare-word (explode "hello") '("h" "e" "l" "l" "o") "o") '("h" "e" "l" "l" "o"))
(check-expect (compare-word (explode "hello") '("h" "e" "l" "l" "o") "a") '("h" "e" "l" "l" "o"))
(check-expect (compare-word (explode "hello") '("_" "_" "_" "_" "_") "v") '("_" "_" "_" "_" "_"))

(define (compare-word word current-status letter)
    (cond
        [(empty? word) current-status]
        [(string=? (first word) letter)
         (cons letter (compare-word (rest word) (rest current-status) letter))]
        [else (cons (first current-status)
                    (compare-word (rest word) (rest current-status) letter))]))


; [List-of linear-combinations] [List-of Number] -> Number
; returns the dot product of the two lists
(check-expect (dot-product '(1 2 3) '(1 2 3)) 14)
(check-expect (dot-product '(1 2 3) '(1 2 3 4)) 14)
(check-expect (dot-product '(1 2 3 4) '(1 2 3)) 14)
(check-expect (dot-product '(1 2 3) '(4 5 6)) 32)
(check-expect (dot-product '(4 5 6) '(1 2 3)) 32)

(define (dot-product l1 l2)
  (cond
    [(empty? l1) 0]
    [(empty? l2) 0]
    [else (+ (* (first l1) (first l2))
             (dot-product (rest l1) (rest l2)))]))


; [List-of String] -> [List-of String]
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
(check-expect (arrangements '("a" "b" "c"))
            (list (list "a" "b" "c")
                (list "b" "a" "c")
                (list "b" "c" "a")
                (list "a" "c" "b")
                (list "c" "a" "b")
                (list "c" "b" "a")))

(check-expect (arrangements '("a" "b")) '(("a" "b") ("b" "a")))
(check-expect (arrangements '("a")) '(("a")))
(check-expect (arrangements '()) (list'()))

(define (arrangements names)
  (cond
    [(empty? names) (list '())]
    [else
     (local (;; String [List-of [List-of String]] -> [List-of [List-of String]]
             (define (insert-everywhere/in-all-words name low)
               (cond
                 [(empty? low) '()]
                 [else (append (insert-everywhere name '() (first low))
                               (insert-everywhere/in-all-words name (rest low)))]))
             ;; String [List-of String] [List-of String] -> [List-of [List-of String]]
             (define (insert-everywhere name prefix suffix)
               (cond
                 [(empty? suffix)
                  (list (append prefix (list name)))]
                 [else
                  (append
                   (list (append prefix (list name) suffix))
                   (insert-everywhere
                    name
                    (append prefix (list (first suffix)))
                    (rest suffix)))])))

       (insert-everywhere/in-all-words (first names)
                                       (arrangements (rest names))))]))


; [NEList-of X] -> X
; returns a random item from the list
(check-random (random-pick '(0 1 2 3 4 5 6 7 8 9)) (random 10))
(check-random (random-pick '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j")) (list-ref '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j") (random 10)))
(check-error (random-pick '()) "random-pick: empty list")

(define (random-pick l)
  (cond
    [(empty? l) (error 'random-pick "empty list")]
    [else (list-ref l (random (length l)))]))

; [List-of String] [List-of [List-of String]]
; ->
; [List-of [List-of String]]
; produces the list of those lists in ll that do
; not agree with names at any place
(check-expect (non-same '("a" "b") '(("a" "b") ("b" "a"))) '(("b" "a")))
(check-expect (non-same '("a" "b") '(("a" "b") ("b" "a") ("a" "a"))) '(("b" "a")))
(check-expect (non-same '("a" "b") '(("a" "b") ("b" "a") ("a" "a") ("b" "b"))) '(("b" "a")))
(check-expect (non-same '() '()) '())
(check-expect (non-same '("am") '(("am"))) '())
(check-expect (non-same '("am") '(("bob"))) '(("bob")))
(check-expect (non-same '("am" "bob") '(("am" "bob") ("bob" "am"))) '(("bob" "am")))
(check-expect (non-same '("am" "bob" "kim") '(("am" "bob" "kim")
                                              ("am" "kim" "bob")
                                              ("bob" "am" "kim")
                                              ("bob" "kim" "am")
                                              ("kim" "am" "bob")
                                              ("kim" "bob" "am")))
              '(("bob" "kim" "am") ("kim" "am" "bob")))

(define (non-same names ll)
  (local ((define (same? names ll)
            (cond
              [(or (empty? names) (empty? ll)) #f]
              [(string=? (first names) (first ll)) #t]
              [else (same? (rest names) (rest ll))])))
    (cond
        [(empty? ll) '()]
        [(same? names (first ll)) (non-same names (rest ll))]
        [else (cons (first ll) (non-same names (rest ll)))])))




(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

; A Schema is a [List-of Spec]

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (make-db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch


(define school-schema
    `(,(make-spec "Name" string?)
      ,(make-spec "Age" integer?)
      ,(make-spec "Present" boolean?)))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define school-db
  (make-db school-schema
           school-content))

(define presence-db
  (make-db presence-schema
           presence-content))


; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check (make-db '() '())) #true)
(check-expect (integrity-check (make-db '() '((1 2 3)))) #false)
(check-expect (integrity-check (make-db '((make-spec "Name" string?)) '((1 2 3)))) #false)

(define (integrity-check db)
  (local ((define schema (db-schema db)); Row -> Boolean
          (define width (length schema))
          (define content (db-content db))
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    width)
                 (andmap (lambda (s c) ((spec-predicate s) c))
                         schema
                         row))))
    (andmap row-integrity-check content)))


; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; applies the predicate to each pair of corresponding items in the lists
(check-expect (andmap2 string=? '() '()) #true)
(check-expect (andmap2 string=? '("a") '("a")) #true)
(check-expect (andmap2 string=? '("a") '("b")) #false)
(check-expect (andmap2 string=? '("a" "b") '("a" "b")) #true)
(check-expect (andmap2 string=? '("a" "b") '("a" "c")) #false)
(check-expect (andmap2 string=? '("a" "b") '("c" "b")) #false)
(check-expect (andmap2 (lambda (x y) (x y)) `(,string? ,boolean?) '(1 #true)) #false)
(check-expect (andmap2 (lambda (x y) (x y)) `(,string? ,boolean?) '("a" 10)) #false)
(check-expect (andmap2 (lambda (x y) (x y)) `(,string? ,boolean?) '("b" #true)) #true)
(check-expect (andmap2 (lambda (x y) (x y)) `(,string? ,boolean?) '("a" #false)) #true)

(define (andmap2 predicate l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) #true]
    [(predicate (first l1) (first l2)) (andmap2 predicate (rest l1) (rest l2))]
    [else #false]))

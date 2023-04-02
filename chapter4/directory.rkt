#lang htdp/isl+

(require htdp/dir)

; A Dir.v1 (short for directory) is one of:
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

; A File.v1 is a String.

(define dir1-text (list "part1" "part2" "part3"))
(define dir1-docs (list "read!"))
(define dir1-code (list "hang" "draw"))
(define dir1-libs (list dir1-docs dir1-code))

(define dir1-ts (list "read!" dir1-text dir1-libs))

; Dir.v1 -> Number
; Counts the number of files in a directory.
(check-expect (count-files '()) 0)
(check-expect (count-files dir1-text) 3)
(check-expect (count-files dir1-libs) 3)
(check-expect (count-files dir1-ts) 7)

(define (count-files d)
  (cond [(null? d) 0]
        [(string? (first d)) (+ 1 (count-files (rest d)))]
        [else (+ (count-files (first d)) (count-files (rest d)))]))




(define-struct dir2 [name content])
; A Dir.v2 is a structure:
;   (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)

; A File.v2 is a String.
 
(define dir2-text (make-dir2 "Text" (list "part1" "part2" "part3")))
(define dir2-docs (make-dir2 "Docs" (list "read!")))
(define dir2-code (make-dir2 "Code"(list "hang" "draw")))
(define dir2-libs (make-dir2 "Libs" (list dir2-docs dir2-code)))

(define dir2-ts (make-dir2 "TS" (list "read!" dir2-text dir2-libs)))

; Dir.v2 -> Number
; Counts the number of files in a directory.
(check-expect (count-files2 dir2-text) 3)
(check-expect (count-files2 dir2-ts) 7)
(check-expect (count-files2 dir2-code) 2)
(check-expect (count-files2 dir2-libs) 3)

(define (count-files2 d)
  (cond [(null? (dir2-content d)) 0]
        [(string? (first (dir2-content d))) (+ 1 (count-files2 (make-dir2 (dir2-name d) (rest (dir2-content d)))))]
        [else (+ (count-files2 (first (dir2-content d))) (count-files2 (make-dir2 (dir2-name d) (rest (dir2-content d)))))]))




(define-struct file.v3 [name size content])
; A File.v3 is a structure:
;   (make-file.v3 String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure:
;   (make-dir.v3 String Dir* File*)

; A Dir* is one of:
; – '()
; – (cons Dir.v3 Dir*)

; A File* is one of:
; – '()
; – (cons File.v3 File*)

(define dir3-text (make-dir.v3 "Text" '() (list (make-file.v3 "part1" 100 "part1 content") (make-file "part2" 200 "part2 content") (make-file "part3" 300 "part3 content"))))
(define dir3-docs (make-dir.v3 "Docs" '() (list (make-file.v3 "read!" 100 "read me!"))))
(define dir3-code (make-dir.v3 "Code" '() (list (make-file.v3 "hang" 100 "hang content") (make-file "draw" 200 "draw content"))))
(define dir3-libs (make-dir.v3 "Libs" (list dir3-docs dir3-code) '()))

(define dir3-ts (make-dir.v3 "TS" (list dir3-text dir3-libs) (list (make-file.v3 "read!" 100 "read me!"))))

; Dir.v3 -> Number
; Counts the number of files in a directory.
(check-expect (count-files3 dir3-text) 3)
(check-expect (count-files3 dir3-ts) 7)
(check-expect (count-files3 dir3-code) 2)

(define (count-files3 d)
  (+ (length (dir.v3-files d))
     (foldr + 0 (map count-files3 (dir.v3-dirs d)))))


(define dir-log (create-dir "/var/log"))

; Dir String -> Boolean
; Checks if a directory contains a file with a given name.
(check-expect (contains-file? dir-log "wtmp") #t)
(check-expect (contains-file? dir-log "thisfiledoesnotexists") #f)

(define (contains-file? d f)
  (local (
          (define (file-in-files? lof f)
            (cond [(null? lof) #f]
                  [(equal? (file-name (first lof)) f) #t]
                  [else (file-in-files? (rest lof) f)]))
          (define (file-in-dirs? lod f)
            (cond [(null? lod) #f]
                  [(file-in-files? (dir-files (first lod)) f) #t]
                  [(file-in-dirs? (dir-dirs (first lod)) f) #t]
                  [else (file-in-dirs? (rest lod) f)])))
    (or (file-in-files? (dir-files d) f) (file-in-dirs? (dir-dirs d) f))))



(define date1 (make-date 2015 1 1 0 0 0))
(define date2 (make-date 2015 1 2 0 0 0))
(define date3 (make-date 2015 1 3 0 0 0))

(define test-subdir2 (make-dir "subdir2" '() (list (make-file "test5" 500 date2 "test5 content"))))
(define test-subdir (make-dir "subdir" (list test-subdir2) (list (make-file "test4" 400 date1 "test4 content"))))

(define test-dir (make-dir "test" (list test-subdir)
                                  (list
                                      (make-file "test1" 100 date1 "test1 content")
                                      (make-file "test2" 200 date2 "test2 content")
                                      (make-file "test3" 300 date3 "test3 content"))))

(define test-dir2 (make-dir "test2" (list test-subdir2)
                                  (list
                                      (make-file "test1" 100 date1 "test1 content")
                                      (make-file "test2" 200 date2 "test2 content")
                                      (make-file "test3" 300 date3 "test3 content"))))
; Dir -> [List-of String]
; Lists the names of all files and directories in a directory.
(check-expect (ls test-dir) (list "test1" "test2" "test3" "subdir"))

(define (ls dir)
  (local (
          (define (ls-files lof)
            (cond [(null? lof) '()]
                  [else (cons (file-name (first lof)) (ls-files (rest lof)))]))
          (define (ls-dirs lod)
            (cond [(null? lod) '()]
                  [else (cons (dir-name (first lod)) (ls-dirs (rest lod)))])))
    (append (ls-files (dir-files dir)) (ls-dirs (dir-dirs dir)))))


; A Path is [List-of String].
; interpretation directions into a directory tree


; Dir String -> Path
; Finds a path to a file with a given name.
; If the file does not exist, returns #f.
(check-expect (find test-dir "test1") (list "test" "test1"))
(check-expect (find test-dir "test4") (list "test" "subdir" "test4"))
(check-expect (find dir-log "akmods.log") (list "log" "akmods" "akmods.log"))

(define (find dir f)
  (local (
          (define (find-in-files lof f)
            (cond [(null? lof) #f]
                  [(equal? (file-name (first lof)) f) (list (file-name (first lof)))]
                  [else (find-in-files (rest lof) f)]))
          (define (find-in-dirs lod f)
            (cond [(null? lod) #f]
                  [(not (false? (find-in-files (dir-files (first lod)) f)))
                   (cons (dir-name (first lod))
                         (find-in-files (dir-files (first lod)) f))]
                  [else (find-in-dirs (rest lod) f)])))
    (if (contains-file? dir f)
        (if (not (false? (find-in-files (dir-files dir) f)))
            (cons (dir-name dir) (find-in-files (dir-files dir) f))
            (cons (dir-name dir) (find-in-dirs (dir-dirs dir) f)))
        #f)))


; Dir -> [String]
; Lists the names of all files in a directory and its subdirectories.
(check-expect (ls-files test-dir) (list "test1" "test2" "test3" "test4"))
(check-expect (ls-files (make-dir "test" '() '())) '())
(check-expect (ls-files (make-dir "test" '() (list (make-file "test1" 100 date1 "test1 content")))) (list "test1"))
(check-expect (ls-files test-dir2) (list "test1" "test2" "test3" "test4" "test5"))

(define (ls-files dir)
  (local (
          (define (ls-files-files lof)
            (cond [(null? lof) '()]
                  [else (cons (file-name (first lof)) (ls-files-files (rest lof)))]))
          (define (ls-files-dirs lod)
            (cond [(null? lod) '()]
                  [else (append (ls-files-files (dir-files (first lod)))
                                (ls-files-dirs (rest lod)))])))
    (append (ls-files-files (dir-files dir)) (ls-files-dirs (dir-dirs dir)))))

;; Author: Jordan Randleman -- files.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(def data-dir (append (path (path-parent #path 2) "examples" "procedures" "io") *file-separator*))
(define io-read-file (append data-dir "read.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS & SYNTAX
(define (temp-file) (path-file (port-path (open-output-file))))

(define (local-temp-path)
  (path #path (temp-file)))

(define-syntax new-fp!
  (lambda ()
    '(begin
      (path-delete! fp)
      (set! fp (local-temp-path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut 
  (file-read io-read-file)
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))

(ut 
  (file-read-string io-read-file)
  "(defn fact\n  ((n) (fact n 1))\n  ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))\n\n(display (fact 5))\n(newline)")

(define fp (local-temp-path))
(ut (begin (file-display fp "\"hello!\"") (file-read fp)) "hello!")
(ut (begin (file-display fp "\"hello!\"") (file-read fp)) "hello!")

(new-fp!)
(ut (begin (file-display+ fp "\"hello!\"") (file-read fp)) "hello!")
(ut (begin (file-display+ fp "\"hello!\"") (file-read fp)) '(begin "hello!" "hello!"))

(new-fp!)
(ut
  (begin
    (file-write 
      fp
      '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
    (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
(ut
  (begin
    (file-write 
      fp
      '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
    (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))

(new-fp!)
(file-write+ fp '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))
(file-write+ fp '(display (fact 5)))
(file-write+ fp '(newline))
(ut
  (begin 'testing-file-write+ (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))

(new-fp!)
(ut
  (begin
    (file-pretty-print 
      fp
      '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
    (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
(ut
  (begin
    (file-pretty-print 
      fp
      '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))
    (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))

(new-fp!)
(file-pretty-print+ fp '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))
(file-pretty-print+ fp '(display (fact 5)))
(file-pretty-print+ fp '(newline))
(ut
  (begin 'testing-file-pretty-print+ (file-read fp))
  '(begin (defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)))

(ut (path? fp) #t)
(ut (path? io-read-file) #t)
(ut (path? #path) #t)
(ut (path? "") #t)
(ut (path? (append #path #path)) #f)
(ut (path? (append fp fp)) #f)
(ut (path? (append io-read-file io-read-file)) #f)

(ut (directory? data-dir) #t)
(ut (directory? #path) #t)
(ut (directory? io-read-file) #f)
(ut (directory? fp) #f)

(ut (file? data-dir) #f)
(ut (file? #path) #f)
(ut (file? io-read-file) #t)
(ut (file? fp) #t)

(define shortlived (local-temp-path))
(file-write shortlived 0)
(ut (file? shortlived) #t)
(ut (file-delete! shortlived) #t)
(ut (file? shortlived) #f)

(new-fp!)
(ut (make-directory fp) #t)
(ut (directory-delete! fp) #t)

(new-fp!)
(ut (make-directory fp) #t)
(set! shortlived (path fp (temp-file)))
(file-write shortlived 0)
(ut (directory-delete! fp) #f) ; verify won't delete directories with anything in them

(ut (directory-recursive-delete! fp) #t)

(set! shortlived (local-temp-path))
(file-write shortlived 0)
(ut (file? shortlived) #t)
(ut (path-delete! shortlived) #t)
(ut (file? shortlived) #f)

(new-fp!)
(ut (make-directory fp) #t)
(ut (path-delete! fp) #t)

(new-fp!)
(ut (make-directory fp) #t)
(set! shortlived (path fp (temp-file)))
(file-write shortlived 0)
(ut (path-delete! fp) #f) ; verify won't delete directories with anything in them

(ut (path-recursive-delete! fp) #t)

(ut 
  (not
    (empty? 
      (filter 
        (lambda (p) 
          (define f (path-file p))
          (and (string? f) (> (length f) 1) (eq? (f 0) #\.)))
        (directory-entries #path))))
  #t)

(ut 
  (begin
    (define entries (directory-entries* #path))
    (and
      (empty? 
        (filter 
          (lambda (p) 
            (define f (path-file p))
            (and (string? f) (> (length f) 1) (eq? (f 0) #\.)))
          entries))
      (not (empty? entries))))
  #t)

(ut (path? (current-directory)) #t)

; testing path
(ut (string-suffix? (path "hi" "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (path "hi" *file-separator* "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (path "hi" *file-separator* *file-separator* "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (path *file-separator* "hi" *file-separator* *file-separator* "there" *file-separator*) (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (path *file-separator* "hi" *file-separator* *file-separator* "there" *file-separator* "man") (append "hi" *file-separator* "there" *file-separator* "man")) #t)
(ut (string-suffix? (path "hi") (append "hi")) #t)
(ut (string-suffix? (path *file-separator* "hi" *file-separator*) "hi") #t)
(ut (path) (current-directory))

; testing / aliases path
(ut (string-suffix? (/ "hi" "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (/ "hi" *file-separator* "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (/ "hi" *file-separator* *file-separator* "there") (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (/ *file-separator* "hi" *file-separator* *file-separator* "there" *file-separator*) (append "hi" *file-separator* "there")) #t)
(ut (string-suffix? (/ *file-separator* "hi" *file-separator* *file-separator* "there" *file-separator* "man") (append "hi" *file-separator* "there" *file-separator* "man")) #t)
(ut (string-suffix? (/ "hi") (append "hi")) #t)
(ut (string-suffix? (/ *file-separator* "hi" *file-separator*) "hi") #t)
(ut (/) (current-directory))

(ut (eq? (path (path-parent io-read-file) "read.scm") io-read-file) #t)
(ut (eq? (path (path-parent io-read-file) (path-file io-read-file)) io-read-file) #t)
(ut (eq? (path (path-parent io-read-file 1) "read.scm") io-read-file) #t)
(ut (eq? (path (path-parent io-read-file 1) (path-file io-read-file)) io-read-file) #t)
(ut (eq? (path (path-parent (path io-read-file "fake_directory") 2) "read.scm") io-read-file) #t)
(ut (eq? (path (path-parent (path io-read-file "fake_directory") 2) (path-file io-read-file)) io-read-file) #t)

(new-fp!)
(define dir1 fp)
(new-fp!)
(define dir2 (path-file fp))
(new-fp!)
(define dir3 (path-file fp))
(define new-dirs (path dir1 dir2 dir3))

(ut (make-directory! new-dirs) #t)
(ut (path-recursive-delete! dir1) #t)

(ut (absolute-path #path) #path)

(ut (absolute-path? #path) #t)

; note <absolute-path?> is not <path?>: it checks string structure, not whether it points to a real file
(ut (absolute-path? (path (path-parent #path) (temp-file))) #t)

(ut (file-extension "read.scm.gzip") "gzip")
(ut (file-extension "read.scm") "scm")
(ut (file-extension "read.") "")
(ut (file-extension "read") #f)

(ut (file-has-extension? "read.scm.gzip" "gzip") #t)
(ut (file-has-extension? "read.scm.gzip" "scm") #f)
(ut (file-has-extension? "read.scm" "scm") #t)
(ut (file-has-extension? "read." "") #t)
(ut (file-has-extension? "read" "") #f)
(ut (file-has-extension? "read" "anything") #f)

(ut (swap-file-extension "read.scm.gzip" "c") "read.scm.c")
(ut (swap-file-extension "read.scm" "java") "read.java")
(ut (swap-file-extension "read." "") "read.")
(ut (swap-file-extension "read" "scm") "read.scm")

(ut (remove-file-extension "read.scm.gzip") "read.scm")
(ut (remove-file-extension "read.scm") "read")
(ut (remove-file-extension "read.") "read")
(ut (remove-file-extension "read") "read")

(ut (> (file-size io-read-file) 0) #t)

(new-fp!)
(define file1 fp)
(new-fp!)
(define file2 fp)
(new-fp!)
(define file3 fp)

(file-write file1 0)
(ut (and (file? file1) (not (file? file2)) (not (file? file3))) #t)

(move-file file1 file2)
(ut (begin 'test-move-file (and (not (file? file1)) (file? file2) (not (file? file3)))) #t)
(file-write file3 1)
(move-file! file2 file3)
(ut (begin 'test-move-file! (and (not (file? file1)) (not (file? file2)) (file? file3))) #t)
(ut (file-read file3) 0)

(define file2-base (local-temp-path))
(set! file2 (path file2-base (temp-file) (temp-file)))
(move-file! file3 file2)
(ut (begin 'test-move-file! (and (not (file? file1)) (file? file2) (not (file? file3)))) #t)
(ut (file-read file2) 0)
(path-recursive-delete! file2-base)

(set! file1 (local-temp-path))
(file-write+ file1 '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))
(file-write+ file1 '(display (fact 5)))
(file-write+ file1 '(newline))

(set! file2 (local-temp-path))
(copy-file file1 file2)
(ut (begin 'testing-copy-file (and (file? file1) (file? file2))) #t)
(ut (begin 'testing-copy-file (file-read file1)) (file-read file2))
(path-delete! file2)

(set! file2-base (local-temp-path))
(set! file2 (path file2-base (temp-file) (temp-file)))
(copy-file! file1 file2)
(ut (begin 'test-copy-file! (and (file? file1) (file? file2) (not (file? file3)))) #t)
(ut (file-read file2) (file-read file1))
(path-delete! file1)
(path-recursive-delete! file2-base)

;; Author: Jordan Randleman -- io.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define in-file (absolute-path (append #path *file-separator* ".." *file-separator* ".." *file-separator* "examples" *file-separator* "procedures" *file-separator* "io" *file-separator* "read.scm")))
(define *in-file-contents*
"(defn fact
  ((n) (fact n 1))
  ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))

(display (fact 5))
(newline)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS & SYNTAX
(define (new-out-port!)
  (close-port! out-port)
  (set! out-port (open-output-file)))

(define (reset-in-port!)
  (close-port! in-port)
  (set! in-port (open-input-file in-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define out-port (open-output-file))
(ut (begin (pretty-print out-port "\"hello\"") (file-read (port-path out-port))) "\"hello\"")

(new-out-port!)
(ut (begin (write out-port "\"hello\"") (file-read (port-path out-port))) "\"hello\"")

(new-out-port!)
(ut (begin (display out-port "\"hello\"") (file-read (port-path out-port))) "hello")

(new-out-port!)
(ut (begin (display out-port "\"hello\"") (file-read (port-path out-port))) "hello")

(new-out-port!)
(ut (begin (newline out-port) (file-read-string (port-path out-port))) "\n")

; Note: more extensive formatting tests are done in <format.scm>. This just tests our
;       ability to output <stringf>'s result to a port via <pprint>, <write>, & <display>
(new-out-port!)
(ut (begin (pretty-printf out-port "%a" "\"hello\"") (file-read (port-path out-port))) "\"hello\"")
(new-out-port!)
(ut (begin (writef out-port "%a" "\"hello\"") (file-read (port-path out-port))) "\"hello\"")
(new-out-port!)
(ut (begin (displayf out-port "%a" "\"hello\"") (file-read (port-path out-port))) "hello")

(define in-port (open-input-file in-file))
(ut (read in-port) '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))
(ut (read in-port) '(display (fact 5)))
(ut (read in-port) '(newline))

(ut (eof? (read in-port)) #t)
(ut (eof? (read in-port)) #t)
(ut (eof? (read in-port)) #t)

(ut 
  (read-string "(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))) (display (fact 5)) (newline)")
  (cons 
    '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))
    "(display (fact 5)) (newline)"))
(ut 
  (read-string "(display (fact 5)) (newline)")
  (cons '(display (fact 5)) "(newline)"))
(ut (read-string "(newline)") (cons '(newline) ""))
(ut (read-string "") #void)
(ut (read-string "    ") #void)

(reset-in-port!)
(ut (read-line in-port) "(defn fact")
(ut (read-line in-port) "  ((n) (fact n 1))")
(ut (read-line in-port) "  ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))")
(ut (read-line in-port) "")
(ut (read-line in-port) "(display (fact 5))")
(ut (read-line in-port) "(newline)")
(ut (eof? (read-line in-port)) #t)
(ut (eof? (read-line in-port)) #t)
(ut (eof? (read-line in-port)) #t)

(reset-in-port!)
(let ((i 0) (n (length *in-file-contents*)))
  (while ((< i n))
    (ut (read-char in-port) (*in-file-contents* i))
    (set! i (+ i 1)))
  (ut (eof? (read-char in-port)) #t)
  (ut (eof? (read-char in-port)) #t)
  (ut (eof? (read-char in-port)) #t))


(reset-in-port!)
(define read-chunk-size 5)
(let ((i 0) (n (length *in-file-contents*)))
  (while ((< i n))
    (ut (read-chars read-chunk-size in-port) (slice *in-file-contents* i read-chunk-size))
    (set! i (+ i read-chunk-size)))
  (ut (eof? (read-chars read-chunk-size in-port)) #t)
  (ut (eof? (read-chars read-chunk-size in-port)) #t)
  (ut (eof? (read-chars read-chunk-size in-port)) #t))

(reset-in-port!) ; show reading 0 chars doesn't move the port
(ut (begin (read-chars 0 in-port) (file-read-string (port-path in-port))) *in-file-contents*)

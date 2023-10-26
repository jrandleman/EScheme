;; Author: Jordan Randleman -- port.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define *in-file-contents*
"(defn fact
  ((n) (fact n 1))
  ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))

(display (fact 5))
(newline)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS
(define (temp-file) (path-file (port-path (open-output-file))))

(define (local-temp-path)
  (path #path (temp-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define in-file (path (path-parent #path 2) "examples" "procedures" "io" "read.scm"))

(define ip (open-input-file in-file))
(ut (begin 'testing-open-input-file (and (open-port? ip) (input-port? ip))) #t)
(ut (begin 'testing-open-input-file (or (closed-port? ip) (output-port? ip))) #f)

(define op1 (open-output-file (local-temp-path)))
(ut (begin 'testing-open-input-file (and (open-port? op1) (output-port? op1))) #t)
(ut (begin 'testing-open-input-file (or (closed-port? op1) (input-port? op1))) #f)
(path-delete! (port-path op1))

(ut (port? ip) #t)
(ut (port? op1) #t)
(ut (port? 0) #f)
(ut (port? in-file) #f)

(define op2 (open-output-file))
(ut (begin 'testing-open-input-file (and (temp-port? op2) (open-port? op2) (output-port? op2))) #t)
(ut (begin 'testing-open-input-file (or (closed-port? op2) (input-port? op2))) #f)

(display op2 123)
(define op3 (open-output-file+ (port-path op2)))
(display op3 456)
(ut (begin 'testing-open-output-file+ (file-read (port-path op2))) 123456)
(ut (and (open-port? op2) (close-port! op2) (closed-port? op2)) #t)
(ut (and (closed-port? op2) (close-port! op2) (closed-port? op2)) #t) ; no problem calling "close-port!" on closed ports

(ut (port-path (open-input-file in-file)) in-file)

(ut (port-position ip) '(1 1))
(ut (begin (read ip) (port-position ip)) '(3 49))

(ut (input-port? (current-input-port)) #t)
(ut (output-port? (current-output-port)) #t)

(call-with-input-file 
  in-file
  (lambda (in-port)
    (ut 
      (begin 'testing-call-with-input-file (read in-port)) 
      '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))))

(call-with-input-file 
  in-file
  (lambda (in-port)
    (ut 
      (begin 'testing-call-with-input-file (read in-port)) 
      '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))))

(define shortlived-file (local-temp-path))
(call-with-output-file
  shortlived-file
  (lambda (out-port)
    (write out-port 123)
    (ut 
      (begin 'testing-call-with-output-file (file-read shortlived-file))
      123)))
(call-with-output-file ; test appends to overwrites files
  shortlived-file
  (lambda (out-port)
    (write out-port 123)
    (ut 
      (begin 'testing-call-with-output-file (file-read shortlived-file))
      123)
    (path-delete! shortlived-file)))

(set! shortlived-file (local-temp-path))
(call-with-output-file+
  shortlived-file
  (lambda (out-port)
    (write out-port 123)
    (ut 
      (begin 'testing-call-with-output-file+ (file-read shortlived-file))
      123)))
(call-with-output-file+ ; test appends to existing files
  shortlived-file
  (lambda (out-port)
    (write out-port 123)
    (ut 
      (begin 'testing-call-with-output-file+ (file-read shortlived-file))
      123123)
    (path-delete! shortlived-file)))

(with-input-from-file 
  in-file
  (lambda ()
    (ut 
      (begin 
        'testing-with-input-from-file 
        (eq? in-file (port-path (current-input-port)))) 
      #t)
    (ut 
      (begin 'testing-with-input-from-file (read))
      '(defn fact ((n) (fact n 1)) ((n p) (if (< n 2) p (fact (- n 1) (* n p))))))))
(ut 
  (begin 
    'testing-with-input-from-file 
    (eq? in-file (port-path (current-input-port)))) 
  #f)

(set! shortlived-file (local-temp-path))
(with-output-to-file 
  shortlived-file
  (lambda ()
    (ut 
      (begin 
        'testing-with-output-to-file 
        (eq? shortlived-file (port-path (current-output-port)))) 
      #t)
    (ut 
      (begin 'testing-with-output-to-file (write 12345) (file-read shortlived-file))
      12345)))
(with-output-to-file ; verify overwrites existing files
  shortlived-file
  (lambda ()
    (ut 
      (begin 
        'testing-with-output-to-file 
        (eq? shortlived-file (port-path (current-output-port)))) 
      #t)
    (ut 
      (begin 'testing-with-output-to-file (write 12345) (file-read shortlived-file))
      12345)))
(ut 
  (begin 
    'testing-with-output-to-file 
    (eq? shortlived-file (port-path (current-output-port)))) 
  #f)
(path-delete! shortlived-file)

(set! shortlived-file (local-temp-path))
(with-output-to-file+ 
  shortlived-file
  (lambda ()
    (ut 
      (begin 
        'testing-with-output-to-file+ 
        (eq? shortlived-file (port-path (current-output-port)))) 
      #t)
    (ut 
      (begin 'testing-with-output-to-file+ (write 12345) (file-read shortlived-file))
      12345)))
(ut 
  (begin 
    'testing-with-output-to-file+ 
    (eq? shortlived-file (port-path (current-output-port)))) 
  #f)
(with-output-to-file+ ; verify appends to existing files
  shortlived-file
  (lambda ()
    (ut 
      (begin 
        'testing-with-output-to-file+ 
        (eq? shortlived-file (port-path (current-output-port)))) 
      #t)
    (ut 
      (begin 'testing-with-output-to-file+ (write 12345) (file-read shortlived-file))
      1234512345)))
(ut 
  (begin 
    'testing-with-output-to-file+ 
    (eq? shortlived-file (port-path (current-output-port)))) 
  #f)
(path-delete! shortlived-file)

(set! ip (open-input-file in-file))

(let ((i 0) (n (length *in-file-contents*)))
  (while ((< i n))
    (ut (peek-port ip) (*in-file-contents* i))
    (read-char ip)
    (set! i (+ i 1)))
  (ut (eof? (peek-port ip)) #t)
  (ut (eof? (peek-port ip)) #t)
  (ut (eof? (peek-port ip)) #t))

(ut (stdin? (current-input-port)) #t)
(ut (stdout? (current-output-port)) #t)
(ut (stdin? ip) #f)
(ut (stdout? op3) #f)
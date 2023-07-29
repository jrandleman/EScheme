;; Author: Jordan Randleman -- serialized.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define data-directory (append (absolute-path (append #path *file-separator* ".." *file-separator* ".." *file-separator* "examples" *file-separator* "procedures" *file-separator* "serialized")) *file-separator*))
(define target-escm-file (append data-directory "target.scm"))
(define target-ser-port (open-output-file)) ; create a temporary file
(define target-ser-file (port-path target-ser-port))

(define target-2-escm-file (append data-directory "target-2.scm"))
(define target-2-ser-port (open-output-file)) ; create a temporary file
(define target-2-ser-file (port-path target-2-ser-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (serialized? target-escm-file) #f)
(ut (begin (serialize target-escm-file target-ser-file) (serialized? target-ser-file)) #t)

; test that serialized files can be loaded & imported
(load target-ser-file)
(import data-directory target)
(ut (fact 5) (target.fact 5))

(ut (serialized? target-2-escm-file) #f)
(ut (begin (serialize-module target-2-escm-file target-2-ser-file) (serialized? target-2-ser-file)) #t)
(ut (defined? target-2-variable) #f)
(load target-2-ser-file)
(ut (defined? target-2-variable) #t)

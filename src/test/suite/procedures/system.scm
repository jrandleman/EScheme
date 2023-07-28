;; Author: Jordan Randleman -- system.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define data-directory (append (absolute-path (append #path *file-separator* ".." *file-separator* ".." *file-separator* "examples" *file-separator* "procedures" *file-separator* "system")) *file-separator*))
(define exit-test-file (append data-directory "exit-test.scm"))
(define load-test-file (append data-directory "load-test.scm"))
(define load-test-2-file (append data-directory "load-test-2.scm"))
(define process-killer-file (append data-directory "process-killer-test.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define system-result (system (append *escm-execution-command* exit-test-file " 0")))
(ut (list? system-result) #t)
(ut (length system-result) 3)
(ut (string? (car system-result)) #t)
(ut (string? (cadr system-result)) #t)
(ut (number? (caddr system-result)) #t)

(ut (caddr (system (append *escm-execution-command* exit-test-file " 0"))) 0)
(ut (caddr (system (append *escm-execution-command* exit-test-file " 1"))) 1)

(ut (< (car (time system 1000 (append *escm-execution-command* process-killer-file))) 60000) #t)

(define *var1* 2)
(ut (begin (load load-test-file) *var2*) (* 2 *var1*))
(set! *var2* #f)
(ut (begin (load data-directory "load-test.scm") *var2*) (* 2 *var1*))

(define load-once-count 0)
(ut (begin (load-once load-test-2-file) *var2*) (* 2 *var1*))
(ut load-once-count 1)
(ut (begin (load-once data-directory "load-test-2.scm") *var2*) (* 2 *var1*))
(ut load-once-count 1)

(import data-directory module-test)
(ut (module? module-test) #t)
(ut (module? 0) #f)
(ut (string? (module-path module-test)) #t)
(ut (list? (module-bindings module-test)) #t)
(ut (module-test.f 4) 8)
(ut (module-test.g 4) 16)
(ut (module-test.*3 4) 12)

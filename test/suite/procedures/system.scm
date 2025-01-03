;; Author: Jordan Randleman -- system.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define data-directory (append (path (path-parent #path 2) "examples" "procedures" "system") *file-separator*))
(define exit-test-file (append data-directory "exit-test.scm"))
(define load-test-file (append data-directory "load-test.scm"))
(define load-test-2-file (append data-directory "load-test-2.scm"))
(define process-killer-file (append data-directory "process-killer-test.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define system-result (system (append *escm-execution-command* "\"" exit-test-file "\"" " 0")))
(ut (list? system-result) #t)
(ut (length system-result) 3)
(ut (string? (car system-result)) #t)
(ut (string? (cadr system-result)) #t)
(ut (number? (caddr system-result)) #t)

(ut (caddr (system (append *escm-execution-command* "\"" exit-test-file "\" 1"))) 1)
(ut (caddr (system (append *escm-execution-command* "\"" exit-test-file "\" 0"))) 0) ; file
(ut (caddr (system (append *escm-execution-command* "\"" exit-test-file "\" 0") '())) 0) ; file + env dir list
(ut (caddr (system (append *escm-execution-command* "\"" exit-test-file "\" 0") data-directory)) 0) ; file + working dir
(ut (caddr (system (append *escm-execution-command* "\"" exit-test-file "\" 0") '() data-directory)) 0) ; file + env dir list + working dir

(ut (< (car (time system 300 (append *escm-execution-command* "\"" process-killer-file "\""))) 60000) #t) ; timeout + file
(ut (< (car (time system 300 (append *escm-execution-command* "\"" process-killer-file "\"") '())) 60000) #t) ; timeout + file+ env dir list
(ut (< (car (time system 300 (append *escm-execution-command* "\"" process-killer-file "\"") data-directory)) 60000) #t) ; timeout + file+ working dir
(ut (< (car (time system 300 (append *escm-execution-command* "\"" process-killer-file "\"") '() data-directory)) 60000) #t) ; timeout + file+ env dir list + working dir

(ut (caddr (escm (append "\"" exit-test-file "\"") 1)) 1)
(ut (caddr (escm (append "\"" exit-test-file "\"") 0)) 0) ; file
(ut (< (car (time escm 300 process-killer-file)) 60000) #t) ; timeout + file

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

(ut (hashmap? (getenv)) #t)
(ut (getenv "") #f)
(let ((env-vars (getenv)))
  (if (> (length env-vars) 0)
      (ut (string? (getenv (car (hashmap-keys env-vars)))) #t)))

(ut (begin 'testing-garbage-collector (garbage-collector) #t) #t) ; just check this is callable (no way to verify effects)

(ut (list? (call-stack)) #t)
(define call-stack (call-stack))
(when (pair? call-stack)
  (define call-stack-1st-function (caar call-stack))
  (define call-stack-1st-source-info (cadar call-stack))
  (ut (begin 'testing-call-stack (string? call-stack-1st-function)) #t)
  (ut (begin 'testing-call-stack (or (boolean? call-stack-1st-source-info) (list? call-stack-1st-source-info))) #t))

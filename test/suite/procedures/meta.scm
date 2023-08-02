;; Author: Jordan Randleman -- meta.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define data-directory (append (path (path-parent #path 2) "examples" "procedures" "system") *file-separator*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(import data-directory module-test)

(ut (apply + (list 1 2 3)) 6)

(ut (eval-bytecode (compile '(if 1 2 3))) 2)
(ut (eval '(if 1 2 3)) 2)

(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym 'abc) (gensym 'abc)) #f)

(ut (syntax? 0) #f)
(ut (syntax? syntax?) #f)
(ut (syntax? if) #t)
(ut (syntax? module-test.f) #f) ; verify works with modules
(ut (syntax? module-test.*3) #t) ; verify works with modules

(ut (list? (expand-syntax '(if 1 2 3))) #t)
(ut (expand-syntax '(module-test.*3 5)) '(* 5 3)) ; verify works with modules

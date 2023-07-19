;; Author: Jordan Randleman -- meta.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (apply + (list 1 2 3)) 6)

(ut (eval-bytecode (compile '(if 1 2 3))) 2)
(ut (eval '(if 1 2 3)) 2)

(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym 'abc) (gensym 'abc)) #f)

(ut (syntax? 0) #f)
(ut (syntax? syntax?) #f)
(ut (syntax? if) #t)

(ut (list? (expand-syntax '(if 1 2 3))) #t)

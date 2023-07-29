;; Author: Jordan Randleman -- pair.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (cons 1 2) '(1 . 2))
(ut (cons (cons 1 2) (cons 3 4)) '((1 . 2) . (3 . 4)))

(ut (car (cons 1 2)) 1)
(ut (cdr (cons 1 2)) 2)

(ut (caar '((1))) 1)
(ut (cadr '(1 2)) 2)
(ut (cdar '((1 2))) '(2))
(ut (cddr '(1 2 3)) '(3))

(ut (caaar '(((1)))) 1)
(ut (caadr '(1 (2))) 2)
(ut (cadar '((1 (2)))) '(2))
(ut (caddr '(1 2 (3))) '(3))
(ut (cdaar '(((1 2)))) '(2))
(ut (cdadr '(1 (2 3))) '(3))
(ut (cddar '((1 2 3))) '(3))
(ut (cdddr '(1 2 3 4)) '(4))

(ut (caaaar '((((1))))) 1)
(ut (caaadr '(1 ((2)))) 2)
(ut (caadar '((1 ((2))))) '(2))
(ut (caaddr '(1 2 ((3)))) '(3))
(ut (cadaar '(((1 (2))))) '(2))
(ut (cadadr '(1 (2 (3)))) '(3))
(ut (caddar '((1 2 (3)))) '(3))
(ut (cadddr '(1 2 3 (4))) '(4))
(ut (cdaaar '((((1 2))))) '(2))
(ut (cdaadr '(1 ((2 3)))) '(3))
(ut (cdadar '((1 (2 3)))) '(3))
(ut (cdaddr '(1 2 (3 4))) '(4))
(ut (cddaar '(((1 2 3)))) '(3))
(ut (cddadr '(1 (2 3 4))) '(4))
(ut (cdddar '((1 2 3 4))) '(4))
(ut (cddddr '(1 2 3 4 5)) '(5))

(ut (pair? (cons 1 2)) #t)
(ut (pair? (list 1 2)) #t)
(ut (pair? '(1 . 2)) #t)
(ut (pair? '(1 2)) #t)
(ut (pair? 0) #f)

(ut (atom? 0) #t)
(ut (atom? atom?) #t)
(ut (atom? (cons 1 2)) #f)
(ut (atom? (list 1 2)) #f)

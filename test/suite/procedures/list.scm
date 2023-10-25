;; Author: Jordan Randleman -- list.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (list) '())
(ut (list 1) '(1))
(ut (list 1 2 3) '(1 2 3))
(ut ls list) ; check alias

(ut (list* 1 2) '(1 . 2))
(ut (list* 1 2) (cons 1 2))
(ut (list* 1 2 3) '(1 2 . 3))
(ut (list* 1 2 3) (cons 1 (cons 2 3)))

(ut (unfold (bind < 5) (bind * 2) (bind + 1) 0) '(0 2 4 6 8 10))
(ut (unfold (bind < 5) (bind * 2) (bind + 1) 6) '())

(ut (unfold-right (bind < 5) (bind * 2) (bind + 1) 0) '(10 8 6 4 2 0))
(ut (unfold-right (bind < 5) (bind * 2) (bind + 1) 6) '())

(ut (memq 0 '(1 2 3 4 5)) #f)
(ut (memq 1 '(1 2 3 4 5)) '(1 2 3 4 5))
(ut (memq 4 '(1 2 3 4 5)) '(4 5))
(ut (memq 3 '(1 2 3 4 5)) '(3 4 5))

(ut (member 0 '(1 2 3 4 5)) #f)
(ut (member 1 '(1 2 3 4 5)) '(1 2 3 4 5))
(ut (member 4 '(1 2 3 4 5)) '(4 5))
(ut (member 3 '(1 2 3 4 5)) '(3 4 5))

(define c (class))
(define o (c))

(ut (memq o '(1 2 3 4 5)) #f)
(ut (memq o (list o 2 3 4 5)) (list o 2 3 4 5))
(ut (memq o (list (c) 2 o 4 5)) (list o 4 5))
(ut (memq o (list (c) 2 3 4 o)) (list o))

(ut (member o '(1 2 3 4 5)) #f)
(ut (member o (list o 2 3 4 5)) (list o 2 3 4 5))
(ut (member o (list (c) 2 o 4 5)) (list (c) 2 o 4 5))
(ut (member o (list (c) 2 3 4 o)) (list (c) 2 3 4 o))

(ut (assq 0 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) #f)
(ut (assq 1 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(1 :a))
(ut (assq 4 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(4 :a))
(ut (assq 3 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(3 :a))

(ut (assoc 0 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) #f)
(ut (assoc 1 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(1 :a))
(ut (assoc 4 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(4 :a))
(ut (assoc 3 '((1 :a) (2 :a) (3 :a) (4 :a) (5 :a))) '(3 :a))

(set! c (class))
(set! o (c))

(ut (assq o (list (list 0 :a) (list 2 :a) (list 3 :a) (list 4 :a) (list 5 :a))) #f)
(ut (assq o (list (list o :a) (list 2 :b) (list 3 :c) (list 4 :d) (list 5 :e))) (list o :a))
(ut (assq o (list (list (c) :a) (list 2 :b) (list o :c) (list 4 :d) (list 5 :e))) (list o :c))
(ut (assq o (list (list (c) :a) (list 2 :b) (list 3 :c) (list 4 :d) (list o :e))) (list o :e))

(ut (assoc o (list (list 0 :a) (list 2 :a) (list 3 :a) (list 4 :a) (list 5 :a))) #f)
(ut (assoc o (list (list o :a) (list 2 :b) (list 3 :c) (list 4 :d) (list 5 :e))) (list o :a))
(ut (assoc o (list (list (c) :a) (list 2 :b) (list o :c) (list 4 :d) (list 5 :e))) (list (c) :a))
(ut (assoc o (list (list (c) :a) (list 2 :b) (list 3 :c) (list 4 :d) (list o :e))) (list (c) :a))

(ut (list? '()) #t)
(ut (list? '(1)) #t)
(ut (list? '(1 2)) #t)
(ut (list? '(1 2)) #t)
(ut (list? "") #f)
(ut (list? []) #f)
(ut (list? {}) #f)

(ut (list*? '()) #f)
(ut (list*? '(1)) #f)
(ut (list*? '(1 2)) #f)
(ut (list*? '(1 . 2)) #t)
(ut (list*? '(1 2 . 3)) #t)

(ut (alist? '()) #t)
(ut (alist? '((a 1))) #t)
(ut (alist? '(a)) #f)
(ut (alist? '((a 1) (b 2))) #t)
(ut (alist? '((a 1) b)) #f)

(ut (null? '()) #t)
(ut (null? #f) #f)
(ut (null? #void) #f)
(ut (null? '(1)) #f)

;; Author: Jordan Randleman -- vector.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (vector) [])
(ut (vector 1) [1])
(ut (vector 1 2 3) [1 2 3])

(ut (make-vector 0 1) [])
(ut (make-vector 1 1) [1])
(ut (make-vector 3 1) [1 1 1])

(define v [0 1 2 3 4 5])
(ut (begin (vector-set! v 0 "0") v) ["0" 1 2 3 4 5])
(ut (begin (vector-set! v 5 "5") v) ["0" 1 2 3 4 "5"])
(ut (begin (vector-set! v 2 "2") v) ["0" 1 "2" 3 4 "5"])

(ut (begin (vector-fill! v 3.14) v) [3.14 3.14 3.14 3.14 3.14 3.14])

(ut (begin (vector-grow! v 0 2.17) v) [3.14 3.14 3.14 3.14 3.14 3.14])
(ut (begin (vector-grow! v 1 2.17) v) [3.14 3.14 3.14 3.14 3.14 3.14 2.17])
(ut (begin (vector-grow! v 2 2.17) v) [3.14 3.14 3.14 3.14 3.14 3.14 2.17 2.17 2.17])

(set! v [])
(ut (begin (vector-grow! v 3 2.17) v) [2.17 2.17 2.17])

(set! v [])
(ut (begin (vector-insert! v 0 5) v) [5])

(set! v [1 2 3])
(ut (begin (vector-insert! v 0 0) v) [0 1 2 3])
(ut (begin (vector-insert! v 4 4) v) [0 1 2 3 4])
(ut (begin (vector-insert! v 3 2.5) v) [0 1 2 2.5 3 4])

(ut (begin (vector-delete! v 0) v) [1 2 2.5 3 4])
(ut (begin (vector-delete! v 4) v) [1 2 2.5 3])
(ut (begin (vector-delete! v 1) v) [1 2.5 3])

(set! v [])
(ut (begin (vector-push! v 0) v) [0])
(ut (begin (vector-push! v 1) v) [0 1])
(ut (begin (vector-push! v 2) v) [0 1 2])

(ut (begin (vector-push-front! v 0) v) [0 0 1 2])
(ut (begin (vector-push-front! v 1) v) [1 0 0 1 2])
(ut (begin (vector-push-front! v 2) v) [2 1 0 0 1 2])

(ut (vector-pop! v) 2)
(ut (vector-pop! v) 1)
(ut (vector-pop! v) 0)

(ut (vector-pop-front! v) 2)
(ut (vector-pop-front! v) 1)
(ut (vector-pop-front! v) 0)

(ut (length v) 0)

(set! v [])
(ut (begin (vector-append! v []) v) [])
(ut (begin (vector-append! v [1] [2] [3]) v) [1 2 3])
(ut (begin (vector-append! v [1 1] [2 2] [3 3]) v) [1 2 3 1 1 2 2 3 3])

(ut (vector-unfold (bind < 5) (bind * 2) (bind + 1) 0) [0 2 4 6 8 10])
(ut (vector-unfold (bind < 5) (bind * 2) (bind + 1) 6) [])

(ut (vector-unfold-right (bind < 5) (bind * 2) (bind + 1) 0) [10 8 6 4 2 0])
(ut (vector-unfold-right (bind < 5) (bind * 2) (bind + 1) 6) [])

(ut (vector-memq [] 0) #f)
(ut (vector-memq [1 2 3 4 5] 0) #f)
(ut (vector-memq [1 2 3 4 5] 1) 0)
(ut (vector-memq [1 2 3 4 5] 4) 3)
(ut (vector-memq [1 2 3 4 5] 3) 2)

(ut (vector-member [] 0) #f)
(ut (vector-member [1 2 3 4 5] 0) #f)
(ut (vector-member [1 2 3 4 5] 1) 0)
(ut (vector-member [1 2 3 4 5] 4) 3)
(ut (vector-member [1 2 3 4 5] 3) 2)

(define c (class))
(define o (c))

(ut (vector-memq [0 2 3 4 5] o) #f)
(ut (vector-memq [o 2 3 4 5] o) 0)
(ut (vector-memq [(c) 2 o 4 5] o) 2)
(ut (vector-memq [(c) 2 3 4 o] o) 4)

(ut (vector-member [0 2 3 4 5] o) #f)
(ut (vector-member [o 2 3 4 5] o) 0)
(ut (vector-member [(c) 2 o 4 5] o) 0)
(ut (vector-member [0 2 o 4 (c)] o) 2)

(ut (vector? []) #t)
(ut (vector? [1]) #t)
(ut (vector? [1 2]) #t)
(ut (vector? [1 2]) #t)
(ut (vector? "") #f)
(ut (vector? '()) #f)
(ut (vector? {}) #f)

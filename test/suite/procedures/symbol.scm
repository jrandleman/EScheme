;; Author: Jordan Randleman -- symbol.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (append 'a) 'a)
(ut (append 'a 'b) 'ab)
(ut (append 'a 'b 'c) 'abc)

(ut (symbol? 'abc) #t)
(ut (symbol? #f) #f)
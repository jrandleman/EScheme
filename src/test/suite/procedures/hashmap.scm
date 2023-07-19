;; Author: Jordan Randleman -- hashmap.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (hashmap) {})
(ut (hashmap :a 1) {:a 1})
(ut (hashmap :a 1 :b 2) {:a 1 :b 2})

(ut (hashmap-keys {}) '())

(ut (sort < (hashmap-keys {1 :a 2 :b 3 :c})) '(1 2 3))
(ut (sort < (hashmap-values {:a 1 :b 2 :c 3})) '(1 2 3))

(ut (hashmap-key? {:a 1 :b 2 :c 3} :d) #f)
(ut (hashmap-key? {:a 1 :b 2 :c 3} :a) #t)

(define hm {:a 1 :b 2 :c 3})
(ut (begin (hashmap-set! hm :d 4) hm) {:a 1 :b 2 :c 3 :d 4})
(ut (begin (hashmap-set! hm :a 99) hm) {:a 99 :b 2 :c 3 :d 4})
(set! hm {})
(ut (begin (hashmap-set! hm :d 4) hm) {:d 4})

(set! hm {:a 1 :b 2 :c 3})
(ut (begin (hashmap-delete! hm :a) hm) {:b 2 :c 3})
(ut (begin (hashmap-delete! hm :c) hm) {:b 2})
(ut (begin (hashmap-delete! hm :b) hm) {})

(ut (hashmap-merge {} {}) {})
(ut (hashmap-merge {:a 1} {}) {:a 1})
(ut (hashmap-merge {} {:a 1}) {:a 1})
(ut (hashmap-merge {:a 1 :c 3} {:b 2 :d 4}) {:a 1 :b 2 :c 3 :d 4})

(set! hm {})
(ut (begin (hashmap-merge! hm {}) hm) {})
(set! hm {:a 1})
(ut (begin (hashmap-merge! hm {}) hm) {:a 1})
(set! hm {})
(ut (begin (hashmap-merge! hm {:a 1}) hm) {:a 1})
(set! hm {:a 1 :c 3})
(ut (begin (hashmap-merge! hm {:b 2 :d 4}) hm) {:a 1 :b 2 :c 3 :d 4})

(ut (hashmap? {}) #t)
(ut (hashmap? {:a 1}) #t)
(ut (hashmap? "") #f)
(ut (hashmap? []) #f)
(ut (hashmap? '()) #f)

(ut (integer? (hashcode 12)) #t)
(ut (integer? (hashcode 12 13 14)) #t)

; testing hashing of value & reference objects
(define v [1])
(ut (hashmap-key? (hashmap '(1) 2) '(1)) #t)
(ut (hashmap-key? (hashmap [1] 2) [1]) #f)
(ut (hashmap-key? (hashmap v 2) v) #t)

(define c (class))
(define o (c))
(ut (hashmap-key? (hashmap (class) 2) (class)) #f)
(ut (hashmap-key? (hashmap c 2) c) #t)
(ut (hashmap-key? (hashmap (c) 2) (c)) #f)
(ut (hashmap-key? (hashmap o 2) o) #t)

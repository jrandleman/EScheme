;; Author: Jordan Randleman -- type-coercion.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (string->number "12") 12)
(ut (string->number "-12") -12)
(ut (string->number "1/4") 3/12)
(ut (string->number "-1/4") -3/12)
(ut (string->number "12.0") 12.0)
(ut (string->number "-12.0") -12.0)
(ut (string->number "-12.0-1.0i") -12.0-1.0i)
(ut (string->number "Infinity") Infinity)
(ut (string->number "-Infinity") -Infinity)
(ut (string->number "NaN") NaN)

(ut (string->number "101" 2) 5)
(ut (string->number "10" 10) 10)

(ut (number->string 5 2) "101")
(ut (number->string 10 10) "10")

(ut (keyword->symbol :ab) 'ab)
(ut (symbol->keyword 'ab) :ab)

(ut (string->symbol "ab") 'ab)
(ut (symbol->string 'ab) "ab")

(ut (string->keyword "ab") :ab)
(ut (keyword->string :ab) "ab")

(ut (write-to-string "h") "\"h\"")
(ut (display-to-string "h") "h")
(ut (pretty-print-to-string "h") "\"h\"")

(ut (vector->list []) '())
(ut (vector->list [1]) '(1))
(ut (vector->list [1 2 3]) '(1 2 3))

(ut (list->vector '()) [])
(ut (list->vector '(1)) [1])
(ut (list->vector '(1 2 3)) [1 2 3])

(list-has-values? (hashmap->list {}))
(list-has-values? (hashmap->list {:a 1}) :a 1)
(list-has-values? (hashmap->list {:a 1 :b 2 :c 3}) :a 1 :b 2 :c 3)

(ut (list->hashmap '()) {})
(ut (list->hashmap '(:a 1)) {:a 1})
(ut (list->hashmap '(:a 1 :b 2 :c 3)) {:a 1 :b 2 :c 3})

(list-has-values? (vector->list (hashmap->vector {})))
(list-has-values? (vector->list (hashmap->vector {:a 1})) :a 1)
(list-has-values? (vector->list (hashmap->vector {:a 1 :b 2 :c 3})) :a 1 :b 2 :c 3)

(ut (vector->hashmap []) {})
(ut (vector->hashmap [:a 1]) {:a 1})
(ut (vector->hashmap [:a 1 :b 2 :c 3]) {:a 1 :b 2 :c 3})

(ut (integer? (char->integer #\1)) #t)
(ut (+ 1 (char->integer #\1)) (char->integer #\2))

(ut (integer->char (char->integer #\1)) #\1)

(ut (list->string '()) "")
(ut (list->string '(#\a)) "a")
(ut (list->string '(#\a #\b)) "ab")

(ut (string->list "") '())
(ut (string->list "a") '(#\a))
(ut (string->list "ab") '(#\a #\b))

(ut (vector->string []) "")
(ut (vector->string [#\a]) "a")
(ut (vector->string [#\a #\b]) "ab")

(ut (string->vector "") [])
(ut (string->vector "a") [#\a])
(ut (string->vector "ab") [#\a #\b])

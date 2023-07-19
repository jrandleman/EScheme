;; Author: Jordan Randleman -- format.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (stringf "") "")

(define-syntax test-token-combination
  (lambda (stringf-expr true-value)
    (define fmt-str (cadr stringf-expr))
    (define fmt-args (cddr stringf-expr))
    `(begin 
      (ut (stringf ,fmt-str ,@fmt-args) ,true-value)
      (ut (stringf (append "*" ,fmt-str) ,@fmt-args) (append "*" ,true-value))
      (ut (stringf (append ,fmt-str "*") ,@fmt-args) (append ,true-value "*"))
      (ut (stringf (append "*" ,fmt-str "*") ,@fmt-args) (append "*" ,true-value "*")))))

(test-token-combination (stringf "%a" "h") "h")
(test-token-combination (stringf "%wa" "h") "\"h\"")
(test-token-combination (stringf "%pa" "h") "\"h\"")

(test-token-combination (stringf "%..." []) "")
(test-token-combination (stringf "%..." ["h"]) "h")
(test-token-combination (stringf "%w..." ["h"]) "\"h\"")
(test-token-combination (stringf "%p..." ["h"]) "\"h\"")
(test-token-combination (stringf "%..." ["h" "h"]) "h h")
(test-token-combination (stringf "%w..." ["h" "h"]) "\"h\" \"h\"")
(test-token-combination (stringf "%p..." ["h" "h"]) "\"h\" \"h\"")
(test-token-combination (stringf "%..." '()) "")
(test-token-combination (stringf "%..." '("h")) "h")
(test-token-combination (stringf "%w..." '("h")) "\"h\"")
(test-token-combination (stringf "%p..." '("h")) "\"h\"")
(test-token-combination (stringf "%..." '("h" "h")) "h h")
(test-token-combination (stringf "%w..." '("h" "h")) "\"h\" \"h\"")
(test-token-combination (stringf "%p..." '("h" "h")) "\"h\" \"h\"")
(test-token-combination (stringf "%..." {}) "")
(test-token-combination (stringf "%..." {:a "h"}) ":a h")
(test-token-combination (stringf "%w..." {:a "h"}) ":a \"h\"")
(test-token-combination (stringf "%p..." {:a "h"}) ":a \"h\"") ; note that we can't predict the printing order for hashmaps with 2+ keys

(test-token-combination (stringf "%n" 12) "12")
(test-token-combination (stringf "%n" -12) "-12")
(test-token-combination (stringf "%n" 3/12) "1/4")
(test-token-combination (stringf "%n" -3/12) "-1/4")
(test-token-combination (stringf "%n" 12.0) "12.0")
(test-token-combination (stringf "%n" -12.0) "-12.0")
(test-token-combination (stringf "%n" -12.0-1i) "-12.0-1.0i")
(test-token-combination (stringf "%n" -12-1.0i) "-12.0-1.0i")
(test-token-combination (stringf "%n" Infinity) "Infinity")
(test-token-combination (stringf "%n" -Infinity) "-Infinity")
(test-token-combination (stringf "%n" NaN) "NaN")

(test-token-combination (stringf "%+n" 12) "+12")
(test-token-combination (stringf "%+n" -12) "-12")
(test-token-combination (stringf "%+n" 3/12) "+1/4")
(test-token-combination (stringf "%+n" -3/12) "-1/4")
(test-token-combination (stringf "%+n" 12.0) "+12.0")
(test-token-combination (stringf "%+n" -12.0) "-12.0")
(test-token-combination (stringf "%+n" -12.0-1i) "-12.0-1.0i")
(test-token-combination (stringf "%+n" -12-1.0i) "-12.0-1.0i")
(test-token-combination (stringf "%+n" Infinity) "+Infinity")
(test-token-combination (stringf "%+n" -Infinity) "-Infinity")
(test-token-combination (stringf "%+n" NaN) "NaN")

(test-token-combination (stringf "%,n" 0) "0")
(test-token-combination (stringf "%,n" 1000) "1,000")
(test-token-combination (stringf "%,n" 1000/1001) "1,000/1,001")
(test-token-combination (stringf "%,n" -1000/1001) "-1,000/1,001")
(test-token-combination (stringf "%+,n" 1000/1001) "+1,000/1,001")

(test-token-combination (stringf "%en" 12.0) "12")
(test-token-combination (stringf "%en" 12e3) "12000")
(test-token-combination (stringf "%in" 12) "12.0")
(test-token-combination (stringf "%in" 1/2) "0.5")

(test-token-combination (stringf "%2rn" 5) "101")
(test-token-combination (stringf "%2Rn" 5) "101")
(test-token-combination (stringf "%10rn" 5) "5")
(test-token-combination (stringf "%10Rn" 5) "5")
(test-token-combination (stringf "%0n" 5) "5")
(test-token-combination (stringf "%1n" 5) "5")
(test-token-combination (stringf "%3n" 5) "005")
(test-token-combination (stringf "%4n" 5) "0005")
(test-token-combination (stringf "%.0n" 3.141592) "3")
(test-token-combination (stringf "%.1n" 3.141592) "3.1")
(test-token-combination (stringf "%.2n" 3.141592) "3.14")
(test-token-combination (stringf "%.4n" 3.141592) "3.1416")

(test-token-combination (stringf "%$" 3.141592) "3.14")
(test-token-combination (stringf "%,$" 3000.141592) "3,000.14")

(test-token-combination (stringf "%s" "\"hello\"") "\"hello\"")
(test-token-combination (stringf "%10s" "\"hello\"") "   \"hello\"")
(test-token-combination (stringf "%-10s" "\"hello\"") "\"hello\"   ")
(test-token-combination (stringf "%ws" "\"hello\"") "\"\\\"hello\\\"\"")

(test-token-combination (stringf "%b" #t) "#t")
(test-token-combination (stringf "%b" #f) "#f")
(test-token-combination (stringf "%wb" #t) "true")
(test-token-combination (stringf "%wb" #f) "false")

(test-token-combination (stringf "%%") "%")

(ut (stringf "%a%a%a" "h" "h" "h") "hhh")
(ut (stringf " %a %a %a " "h" "h" "h") " h h h ")

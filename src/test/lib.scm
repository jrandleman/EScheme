;; Author: Jordan Randleman -- lib.scm
;; => Defines EScheme's Stdlib Testing Suite Helper Macros/Functions.
;;    Used by ./suite/*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TEST MACRO
;;   (unit-test <test-expr> <correct-result>)
;;   (unit-test <eq=?> <test-expr> <correct-result>)
(define-syntax unit-test
  (fn 
    ((test-expr expected-result)
      (list 'let (list (list 'expr test-expr) (list 'result expected-result))
        (list 'unless (list 'equal? 'expr 'result)
          (list 'errorf "UNIT-TEST> Test Expression '%wa [= %wa] did not equate '%wa\n" (list 'quote test-expr) 'expr (list 'quote expected-result)))))
    ((=? test-expr expected-result)
      (list 'let (list (list 'expr test-expr) (list 'result expected-result))
        (list 'unless (list 'or (list =? 'expr 'result) (list 'and (list 'nan? 'expr) (list 'nan? 'result)))
          (list 'errorf "UNIT-TEST> Test Expression '%wa [= %wa] did not '%wa '%wa\n" (list 'quote test-expr) 'expr (list 'quote =?) (list 'quote expected-result)))))))

(define ut unit-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMERIC UNIT TEST MACRO
;;   (numeric-unit-test <test-expr> <correct-result>)
;;   (numeric-unit-test <eq=?> <test-expr> <correct-result>)
;;
;; => Optimized for comparisons against Chez-Scheme's generated answers.
;;    Chops numbers to 6 digits of precision & only warns of NaN differences
;;    (not an error!) to account for weirdness in float/double precision math.
;;      * NaN diffs w/ Chez Scheme have been manually reviewed, and generally
;;        occur due to dealing with complex numbers that include Infinity.
(define *precision-digits-after-decimal* 6)

(define *range-of-sci-digits-to-keep* 
  (+ *precision-digits-after-decimal* 1))

(define *precision-sci-notation-scalar*
  (expt 10.0 *precision-digits-after-decimal*))

(define *minimum-non-zero-value*
  (* 5.0 (expt 10.0 (- *range-of-sci-digits-to-keep*))))

(define (remove-scientific-notation n)
  (if (exact? n) (set! n (exact->inexact n)))
  (define components (string-split (string-upcase (stringf "%n" n)) "E"))
  (if (null? (cdr components))     
      n
      (if (< n *minimum-non-zero-value*)
          0.0
          (/ n (string->number (append "1e" (cadr components)))))))

(define (chop-decimal-to-6-digits n)
  (define components (string-split (stringf "%n" n) "\\."))
  (define chopped-to-7-digits (string->number (append (car components) "." (slice (cadr components) 0 *range-of-sci-digits-to-keep*))))
  (/ (round (* chopped-to-7-digits *precision-sci-notation-scalar*)) *precision-sci-notation-scalar*))

(define (chop-real-to-6-decimal-digits n)
  (if (finite? n)
      (chop-decimal-to-6-digits (remove-scientific-notation n))
      n))

(define (chop-to-6-precision-digits n other) ; account for 6 digits of precision for inexacts
  (if (or (inexact? n) (inexact? other))
      (if (complex? n)
          (make-rectangular 
            (chop-real-to-6-decimal-digits (real-part n)) 
            (chop-real-to-6-decimal-digits (imag-part n)))
          (chop-real-to-6-decimal-digits n))
      n))

(define (number-coercion:escm->chez n)
  (def s (stringf "%wa" n))
  (set! s (string-replace s "-Infinity" "-inf.0"))
  (set! s (string-replace s "\\+Infinity" "+inf.0"))
  (set! s (string-replace s "Infinity" "+inf.0"))
  (set! s (string-replace s "NaN" "+nan.0"))
  s)

(define-syntax numeric-unit-test
  (fn 
    ((test-expr expected-result)
      (list 'let (list (list 'expr test-expr) (list 'result expected-result))
        (list 'let (list (list 'val-1 (list 'chop-to-6-precision-digits 'expr 'result)) 
                         (list 'val-2 (list 'chop-to-6-precision-digits 'result 'expr)))
          (list 'unless (list 'equal? 'val-1 'val-2)
            (list 'errorf "NUMERIC-UNIT-TEST> Test Expression '%wa [= %wa] did not equate '%wa\n                                            '%a [= %a] vs '%a\n" 
              (list 'quote test-expr) 'expr (list 'quote expected-result)
              (list 'number-coercion:escm->chez (list 'quote test-expr)) (list 'number-coercion:escm->chez 'expr) (list 'number-coercion:escm->chez (list 'quote expected-result)))))))
    ((=? test-expr expected-result)
      (list 'let (list (list 'expr test-expr) (list 'result expected-result))
        (list 'let (list (list 'val-1 (list 'chop-to-6-precision-digits 'expr 'result)) 
                         (list 'val-2 (list 'chop-to-6-precision-digits 'result 'expr)))
          (list 'unless (list 'or (list =? 'val-1 'val-2) (list 'and (list 'nan? 'expr) (list 'nan? 'result)))
            (list 'if (list 'or (list 'nan? 'expr) (list 'nan? 'result))
                (list 'displayf 
                      "NUMERIC-UNIT-TEST> Warning: NaN Difference: '%wa [= %wa] vs '%wa\n                                            '%a [= %a] vs '%a\n\n" 
                      (list 'quote test-expr) 'expr (list 'quote expected-result)
                      (list 'number-coercion:escm->chez (list 'quote test-expr)) (list 'number-coercion:escm->chez 'expr) (list 'number-coercion:escm->chez (list 'quote expected-result)))
                (list 'errorf "NUMERIC-UNIT-TEST> Test Expression '%wa [= %wa] did not '%wa '%wa\n                                                       '%a [= %a] vs '%a\n" 
                  (list 'quote test-expr) 'expr (list 'quote =?) (list 'quote expected-result)
                  (list 'number-coercion:escm->chez (list 'quote test-expr)) (list 'number-coercion:escm->chez 'expr) (list 'number-coercion:escm->chez (list 'quote expected-result))))))))))

(define nut numeric-unit-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST SEARCH UTILITY
;;   (list-has-values? <list-expr> <value1> ...)
;;
;; => Useful for when we don't know the order of a list's contents, but know
;;    the values said contents should have.
(define-syntax list-has-values?
  (lambda (lis . args)
    (define is (gensym))
    (cons 'let 
      (cons
        (list (list is lis))
          (map
            (lambda (arg)
              (list 'ut (list 'if (list 'member arg is) #t #f) #t))
            args)))))

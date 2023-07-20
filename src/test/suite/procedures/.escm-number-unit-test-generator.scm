;; Author: Jordan Randleman
;; Purpose: Chez-Scheme script to generate numeric unit tests for EScheme.
;;          The numeric tower is extensive enough such that fully writing these
;;          tests by hand would be too painful, hence we generate them!
;;          => Note that this isn't used by EScheme directly, but rather just in
;;             a dev environment w/ access to Chez Scheme to help debug the EScheme
;;             VM (Chez Scheme is NOT a prerequisite for EScheme use!).
;; Execution: Run this file in the same directory it is kept!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER CONSTANTS
(define *file-separator* 
  (let ((cwd (directory-separator)))
    (if (char? cwd)
        (list->string (list cwd))
        cwd)))

(define ESCM_UNIT_TESTING_LIBRARY_PATH (string-append (current-directory) *file-separator* ".." *file-separator* ".." *file-separator* "lib.scm"))

(define GENERATED_TEST_DIRECTORY_PATH (string-append (current-directory) *file-separator* "number-tests"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TEST COUNTER
(define unit-test-count 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFMACRO HELPER
;; From: https://gist.github.com/yuhr/bba4de4b34db940dcfba2b756f6cbc32
(define-syntax defmacro
  (syntax-rules ()
    ((k (name . args) body ...)
     (defmacro name (lambda args body ...)))
    ((k name transformer)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           ((l . sv)
            (let* ((v (syntax->datum (syntax sv)))
                   (e (apply transformer v)))
              (if (eq? (void) e)
                  (syntax (void))
                  (datum->syntax (syntax l) e))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRST HELPER (FOR ESCM'S MATH DIFFERENCES W/ CHEZ-SCHEME)
;; (first <expr> ...) ; returns the first non-#f <expr>, or #f if all #f.
(define (id x) x)

(defmacro (first . args)
  (append 
    (cons 'cond (map (lambda (arg) (list arg '=> 'id)) args))
    (list (list #t #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTING HELPER
(define (print x . xs)
  (if (output-port? x)
      (begin
        (for-each (lambda (a) (display a x)) xs) 
        (newline x))
      (begin 
        (for-each (lambda (a) (display a)) (cons x xs))
        (newline))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM COMPLEX IDENTIFICATION
(define (number-has-nan? n)
  (if (real? n)
      (nan? n)
      (and (complex? n) (or (nan? (real-part n)) (nan? (imag-part n))))))

; For some reason by default Chez does goofy things like calling 0 <complex>
(define (escm-complex? n)
  (and (not (number-has-nan? n)) 
       (complex? n) 
       (not (zero? (imag-part n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM COMPLEX SQRT NAN-SEMANITCS
;; (sqrt [+-]inf[+-]infi) ; +nan.0
(define chez-sqrt sqrt)

(define (sqrt x)
  (if (and (complex? x) (infinite? (real-part x)) (infinite? (imag-part x)))
      +nan.0
      (chez-sqrt x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM ROUND SEMANITCS
;; (round 1/2) ; 1
(define chez-round round)

(define (round n)
  (if (= n 1/2) 1 (chez-round n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM ASINH SEMANTICS
;; => Despite running afoul of both Chez-Scheme & Guile, we can confirm this 
;;    result by entering "asinh(0-2i)=" into Wolfram Alpha:
;;    * https://www.wolframalpha.com/input?i2d=true&i=asinh%5C%2840%290-2i%5C%2841%29%3D
;; (asinh 0.0-2.0i) ; -1.3169578969248166-1.5707963267948966i
(define chez-asinh asinh)

(define (asinh n)
  (if (= n 0.0-2.0i) -1.3169578969248166-1.5707963267948966i (chez-asinh n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM NPR EQUIVALENT
(define (npr-factorial n)
  (define (iter n p)
    (if (< n 2)
        p
        (iter (- n 1) (* n p))))
  (iter n 1))

(define (npr n r)
  (/ (npr-factorial n) (npr-factorial (- n r))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM NCR EQUIVALENT
(define (ncr n r)
  (/ (npr-factorial n) (* (npr-factorial r) (npr-factorial (- n r)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM DIVREM EQUIVALENT
(define (divrem a b) (cons (quotient a b) (remainder a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST PERMUTATION HELPER FUNCTIONS
;; From: https://stackoverflow.com/a/61421721
(define (get-list-permutations ls)
  (define (insert-perm x ls)
    (if (null? ls)
        (list (list x))
        (cons (cons x ls)
              (map (lambda (l) (cons (car ls) l))
                   (insert-perm x (cdr ls))))))
  (if (null? ls)
      '(())
      (apply append
             (map (lambda (l) (insert-perm (car ls) l))
                  (get-list-permutations (cdr ls))))))

;; From: https://rosettacode.org/wiki/Combinations#Scheme
(define (get-list-combinations m lst)
  (cond ((= m 0) '(()))
        ((null? lst) '())
        (else (append (map (lambda (y) (cons (car lst) y))
                           (get-list-combinations (- m 1) (cdr lst)))
                      (get-list-combinations m (cdr lst))))))

(define (get-list-argument-variations sample-size lst)
  (apply append (map get-list-permutations (get-list-combinations sample-size lst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM-STYLE INFINITY->STRING SYNTAX HELPERS
(define (create-escm-real n)
  (if (real? n)
      n
      (real-part n)))

(define (escm-inf-string n)
  (if (negative? (create-escm-real n)) "-Infinity" "Infinity"))

(define (escm-infinite? n)
  (and (number? n)
       (or (and (real? n) (infinite? n))
           (and (complex? n) (zero? (imag-part n)) (infinite? (real-part n))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM-STYLE NUMBER->STRING SYNTAX HELPERS
;; => Helps w/ NaN & Infinity conversions
(define (escm-number->string n)
  (define i-string #f)
  (cond ((number-has-nan? n) "NaN")
        ((and (number? n) (escm-complex? n))
          (let ((r (real-part n)) (i (imag-part n)))
            (cond ((infinite? r)
                    (if (infinite? i) 
                        (set! i-string (escm-inf-string i))
                        (set! i-string (number->string i)))
                    (if (negative? i)
                        (string-append (escm-inf-string r) i-string "i")
                        (string-append (escm-inf-string r) "+" i-string "i")))
                  ((infinite? i)
                    (if (negative? i)
                        (string-append (number->string r) (escm-inf-string i) "i")
                        (string-append (number->string r) "+" (escm-inf-string i) "i")))
                  (else
                    (number->string n)))))
        ((escm-infinite? n)
          (escm-inf-string n))
        (else
          (format "~a" n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPERATION ARGUMENT COMBINATIONS GENERATION
(define *real-values-set*
  '((0 0.0)

    (+inf.0 -inf.0 +nan.0)

    (1 4)
    (-1 -4)

    (1.0 4.0)
    (-1.0 -4.0)

    (1/2 1/8)
    (-1/2 -1/8)))

(define *complex-values-set*
  '((+i -i)

    (0+2i 0.0+2.0i 0+1/2i)
    (0-2i 0.0-2.0i 0-1/2i)
    (0.0+inf.0i 0.0-inf.0i)

    (1+4i 2+8i)
    (1-4i 2-8i)
    (-1+4i -2+8i)
    (-1-4i -2-8i)

    (1.0+4.0i 2.0+8.0i)
    (1.0-4.0i 2.0-8.0i)
    (-1.0+4.0i -2.0+8.0i)
    (-1.0-4.0i -2.0-8.0i)

    (1/2+3/4i 3/8+5/8i)
    (1/2-3/4i 3/8-5/8i)
    (-1/2+3/4i -3/8+5/8i)
    (-1/2-3/4i -3/8-5/8i)

    (+inf.0+1.0i +inf.0+4.0i)
    (+inf.0-1.0i +inf.0-4.0i)
    (-inf.0+1.0i -inf.0+4.0i)
    (-inf.0-1.0i -inf.0-4.0i)

    (1.0+inf.0i 4.0+inf.0i)
    (1.0-inf.0i 4.0-inf.0i)
    (-1.0+inf.0i -4.0+inf.0i)
    (-1.0-inf.0i -4.0-inf.0i)

    (+inf.0+inf.0i -inf.0+inf.0i +inf.0-inf.0i -inf.0-inf.0i)))

(set! *complex-values-set* (apply append *complex-values-set*))

(set! *real-values-set* (apply append *real-values-set*))
(define *non-NaN-real-values-set* (filter (lambda (x) (not (number-has-nan? x))) *real-values-set*))

(define *values-set* (append *real-values-set* *complex-values-set*))
(define *non-NaN-values-set* (filter (lambda (x) (not (number-has-nan? x))) *values-set*))

(define *exact-values-set* (filter exact? *values-set*))

(define *inexact-values-set* (filter inexact? *values-set*))
(define *non-NaN-inexact-values-set* (filter (lambda (x) (not (number-has-nan? x))) *inexact-values-set*))

(define *non-negative-integer-values-set* (filter (lambda (x) (and (integer? x) (not (negative? x)))) *values-set*))

(defmacro (create-caching-value-combination-generator value-set)
  `(let ((memoized-values '()))
    (lambda (arg-count)
      (define result (assq arg-count memoized-values))
      (if result
          (cdr result)
          (let ((vals (get-list-argument-variations arg-count ,value-set)))
            (set! memoized-values (cons (cons arg-count vals) memoized-values))
            vals)))))

(define generate-values-matrix 
  (create-caching-value-combination-generator *values-set*))

(define generate-non-NaN-values-matrix 
  (create-caching-value-combination-generator *non-NaN-values-set*))

(define generate-real-values-matrix 
  (create-caching-value-combination-generator *real-values-set*))

(define generate-non-NaN-real-values-matrix 
  (create-caching-value-combination-generator *non-NaN-real-values-set*))

(define generate-complex-values-matrix 
  (create-caching-value-combination-generator *complex-values-set*))

(define generate-inexact-values-matrix 
  (create-caching-value-combination-generator *inexact-values-set*))

(define generate-non-NaN-inexact-values-matrix 
  (create-caching-value-combination-generator *non-NaN-inexact-values-set*))

(define generate-exact-values-matrix 
  (create-caching-value-combination-generator *exact-values-set*))

(define generate-non-negative-integer-values-matrix 
  (create-caching-value-combination-generator *non-negative-integer-values-set*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM/CHEZ DIFFERENCE: DIVIDE BY 0 HELPER
;; => div by 0 can yield an Infinity, not always NaN
(define (get-/0-result-from-lhs n)
  (cond ((or (number-has-nan? n) (zero? n)) +nan.0)
        ((negative? n) -inf.0)
        (else +inf.0)))

(define (div-by-0? op args)
  (if (eq? op '/)
      (if (null? (cdr args))
          (if (= (car args) 0) +inf.0 #f)
          (if (= (cadr args) 0) 
              (let ((lhs (car args)))
                (if (real? lhs)
                    (get-/0-result-from-lhs lhs)
                    (let ((r (real-part lhs)) (i (imag-part lhs)))
                      (define rinf (get-/0-result-from-lhs r))
                      (define iinf (get-/0-result-from-lhs i))
                      (if (or (nan? rinf) (nan? iinf))
                          +nan.0
                          (make-rectangular rinf iinf)))))
              #f))
      #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM/CHEZ DIFFERENCE: (/ 0 <complex>)
;; => Chez-Scheme says +nan.0, EScheme says 0.0
;;    * Note we can manually verify 0.0 making sense via:
;;        (a+bi)/(c+di) = [(ac+bd)/(cc+dd)]+[(bc-ad)/(cc+dd)]i
;;                      |- a=0, b=0, c=I, d=1
;;                      = [(0c+0d)/(cc+dd)]+[(0c-0d)/(cc+dd)]i
;;                      = [0/(cc+dd)]+[0/(cc+dd)]i
;;                      = 0
(define (div-0-by-escm-complex? op args)
  (if (eq? op '/)
      (if (null? (cdr args))
          #f
          (if (and (= (car args) 0) (escm-complex? (cadr args)))
              0.0
              #f))
      #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCM/CHEZ DIFFERENCE: (<non-*-op> <number> NaN) = (<non-*-op> NaN <number>) = NaN
(define (op-accepts-NaN? op)
  (case op ((* expt expt-mod) #t) (else #f)))


(define (invalid-procedure-has-NaN-args? =? op args)
  (if (or (not (eq? =? '=)) (op-accepts-NaN? op))
      #f
      (if (positive? (length (filter number-has-nan? args)))
          +nan.0
          #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TEST GENERATOR
(define (escm-op-special-case? =? op args)
  (first 
    (invalid-procedure-has-NaN-args? =? op args)
    (div-0-by-escm-complex? op args)
    (div-by-0? op args)))

(define *escm-failed-op-evaluation* '*escm-failed-op-evaluation*)

(define (coerce-inexact! args)
  (if (positive? (length (filter inexact? args)))
      (let loop ((ls args))
        (if (pair? ls)
            (begin 
              (if (exact? (car ls)) (set-car! ls (exact->inexact (car ls))))
              (loop (cdr ls)))))))

(define (eval-op =? op args)
  (coerce-inexact! args) ; important since chez-scheme is goofy with these coercions
  (let ((escm-val (escm-op-special-case? =? op args)))
    (let ((op-result 
            (if escm-val 
                escm-val 
                (guard (condition (else *escm-failed-op-evaluation*)) 
                       (apply (eval op) args)))))
      (if (number-has-nan? op-result) +nan.0 op-result))))

(define (generate-test =? op args)
  (let ((correct-test-result (eval-op =? op args)))
    (if (eq? correct-test-result *escm-failed-op-evaluation*)
        ':nop
        (begin
          (set! unit-test-count (+ unit-test-count 1))
          `(nut ,=? (,op ,@(map escm-number->string args)) ',(escm-number->string correct-test-result))))))

(defmacro (generate-unit-tests =? file-name op arg-count values-generator-function)
  (define out-port
    (open-output-file 
      (string-append GENERATED_TEST_DIRECTORY_PATH *file-separator* file-name "-" (number->string arg-count) ".scm")
      'replace))
  (write (list 'load ESCM_UNIT_TESTING_LIBRARY_PATH) out-port)
  (newline out-port)
  (for-each 
    (lambda (args) (print out-port (generate-test =? op args)))
    ((eval values-generator-function) arg-count))
  (close-output-port out-port))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DELETE PREVIOUSLY GENERATED FILES
(define (delete-directory-files! dirname)
  (when (file-directory? dirname)
    (for-each 
      (lambda (file)
        (set! file (string-append dirname *file-separator* file))
        (unless (file-directory? file)
          (delete-file file)))
      (directory-list dirname))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN EXECUTION
(delete-directory-files! GENERATED_TEST_DIRECTORY_PATH)

(generate-unit-tests = "plus" + 1 generate-values-matrix)
(generate-unit-tests = "plus" + 2 generate-values-matrix)

(generate-unit-tests = "sub" - 1 generate-values-matrix)
(generate-unit-tests = "sub" - 2 generate-values-matrix)

(generate-unit-tests = "mul" * 1 generate-values-matrix)
(generate-unit-tests = "mul" * 2 generate-values-matrix)

(generate-unit-tests = "div" / 1 generate-values-matrix)
(generate-unit-tests = "div" / 2 generate-values-matrix)

(generate-unit-tests equal? "equals" = 2 generate-real-values-matrix)
(generate-unit-tests equal? "less_than" < 2 generate-real-values-matrix)
(generate-unit-tests equal? "greater_than" > 2 generate-real-values-matrix)
(generate-unit-tests equal? "less_than_equal" <= 2 generate-real-values-matrix)
(generate-unit-tests equal? "greater_than_equal" >= 2 generate-real-values-matrix)

(generate-unit-tests = "expt" expt 2 generate-values-matrix)

(generate-unit-tests = "exp" exp 1 generate-values-matrix)

(generate-unit-tests = "log" log 1 generate-values-matrix)
(generate-unit-tests = "log" log 2 generate-values-matrix)

(generate-unit-tests = "sqrt" sqrt 1 generate-values-matrix)

(generate-unit-tests = "abs" abs 1 generate-real-values-matrix)

(generate-unit-tests = "expt_mod" expt-mod 3 generate-real-values-matrix)

(generate-unit-tests = "min" min 1 generate-real-values-matrix)
(generate-unit-tests = "max" max 1 generate-real-values-matrix)
(generate-unit-tests = "min" min 2 generate-real-values-matrix)
(generate-unit-tests = "max" max 2 generate-real-values-matrix)
(generate-unit-tests = "min" min 3 generate-real-values-matrix)
(generate-unit-tests = "max" max 3 generate-real-values-matrix)

(generate-unit-tests = "exact_to_inexact" exact->inexact 1 generate-exact-values-matrix)
(generate-unit-tests = "inexact_to_exact" inexact->exact 1 generate-non-NaN-inexact-values-matrix)

(generate-unit-tests = "numerator" numerator 1 generate-non-NaN-real-values-matrix)
(generate-unit-tests = "denominator" denominator 1 generate-non-NaN-real-values-matrix)

(generate-unit-tests = "quotient" quotient 2 generate-real-values-matrix)
(generate-unit-tests = "remainder" remainder 2 generate-real-values-matrix)
(generate-unit-tests = "modulo" modulo 2 generate-real-values-matrix)

(generate-unit-tests equal? "divrem" divrem 2 generate-real-values-matrix)

(generate-unit-tests = "gcd" gcd 2 generate-non-negative-integer-values-matrix)
(generate-unit-tests = "lcm" lcm 2 generate-non-negative-integer-values-matrix)

(generate-unit-tests = "round" round 1 generate-real-values-matrix)
(generate-unit-tests = "floor" floor 1 generate-real-values-matrix)
(generate-unit-tests = "ceiling" ceiling 1 generate-real-values-matrix)
(generate-unit-tests = "truncate" truncate 1 generate-real-values-matrix)

(generate-unit-tests equal? "is_number" number? 1 generate-values-matrix)

(generate-unit-tests equal? "is_complex" complex? 1 generate-values-matrix)
(generate-unit-tests equal? "is_real" real? 1 generate-values-matrix)

(generate-unit-tests equal? "is_inexact" inexact? 1 generate-values-matrix)
(generate-unit-tests equal? "is_exact" exact? 1 generate-values-matrix)

(generate-unit-tests equal? "is_integer" integer? 1 generate-values-matrix)

(generate-unit-tests equal? "is_finite" finite? 1 generate-real-values-matrix)
(generate-unit-tests equal? "is_infinite" infinite? 1 generate-real-values-matrix)
(generate-unit-tests equal? "is_nan" nan? 1 generate-real-values-matrix)

(generate-unit-tests equal? "is_odd" odd? 1 generate-real-values-matrix)
(generate-unit-tests equal? "is_even" even? 1 generate-real-values-matrix)

(generate-unit-tests equal? "is_zero" zero? 1 generate-real-values-matrix)
(generate-unit-tests equal? "is_positive" positive? 1 generate-real-values-matrix)
(generate-unit-tests equal? "is_negative" negative? 1 generate-real-values-matrix)

(generate-unit-tests equal? "sin" sin 1 generate-values-matrix)
(generate-unit-tests equal? "cos" cos 1 generate-values-matrix)
(generate-unit-tests equal? "tan" tan 1 generate-values-matrix)

(generate-unit-tests equal? "asin" asin 1 generate-values-matrix)
(generate-unit-tests equal? "acos" acos 1 generate-values-matrix)
(generate-unit-tests equal? "atan" atan 1 generate-values-matrix)

(generate-unit-tests equal? "atan" atan 2 generate-real-values-matrix)

(generate-unit-tests equal? "sinh" sinh 1 generate-values-matrix)
(generate-unit-tests equal? "cosh" cosh 1 generate-values-matrix)
(generate-unit-tests equal? "tanh" tanh 1 generate-values-matrix)

(generate-unit-tests equal? "asinh" asinh 1 generate-values-matrix)
(generate-unit-tests equal? "acosh" acosh 1 generate-values-matrix)
(generate-unit-tests equal? "atanh" atanh 1 generate-values-matrix)

(generate-unit-tests = "npr" npr 2 generate-non-negative-integer-values-matrix)
(generate-unit-tests = "ncr" ncr 2 generate-non-negative-integer-values-matrix)

(generate-unit-tests = "make_rectangular" make-rectangular 2 generate-real-values-matrix)
(generate-unit-tests = "make_polar" make-polar 2 generate-real-values-matrix)

(generate-unit-tests equal? "real_part" real-part 1 generate-values-matrix)
(generate-unit-tests equal? "imag_part" imag-part 1 generate-values-matrix)

(generate-unit-tests equal? "magnitude" magnitude 1 generate-values-matrix)
(generate-unit-tests equal? "angle" angle 1 generate-values-matrix)

(generate-unit-tests equal? "conjugate" conjugate 1 generate-values-matrix)

(display (format ">> Generated ~a tests!\n" unit-test-count))

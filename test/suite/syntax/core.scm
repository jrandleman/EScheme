;; Author: Jordan Randleman -- core.scm
;; => Tests for EScheme's primitive core syntax.
;;    Invoked by ../../main.scm
;; => Realistically most of these working predicate unit tests executing at
;;    all, but we still fully exercise the forms to validate corner cases.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (bytecode (load 1)) 1)


(ut (quote 1) 1)
(ut (quote (1 2)) (list 1 2))
(ut (quote (1 . b)) (cons 1 'b))
(ut (quote [1 b]) (vector 1 'b))
(ut (quote {1 b}) (hashmap 1 'b))


(define-syntax macro-test
  (lambda (a b)
    (list (quote +) a b)))
(ut (macro-test 1 2) 3)


; arity dispatch 
(ut ((fn ((x . xs) xs)) 1) '())
(ut ((fn ((x . xs) xs)) 1 2) '(2))
(ut ((fn ((x . xs) xs)) 1 2 3) '(2 3))
(ut ((fn (() 1) ((a) a) (xs xs))) 1)
(ut ((fn (() 1) ((a) a) (xs xs)) 2) 2)
(ut ((fn (() 1) ((a) a) (xs xs)) 2 3) '(2 3))
; optional values
(ut ((fn (((a 1)) a))) 1)
(ut ((fn (((a 1)) a)) 2) 2)
(ut ((fn (((a 1) (b 2)) (+ a b)))) 3)
(ut ((fn (((a 1) (b 2)) (+ a b))) 2) 4)
(ut ((fn (((a 1) (b 2)) (+ a b))) 2 3) 5)
(ut ((fn (((a 1) (b 2)) (+ a b)) ((a b c) (+ a b c))) 2 3 4) 9)
(ut ((fn (() 99) ((a (b 1)) (+ a b)))) 99)
(ut ((fn (() 99) ((a (b 1)) (+ a b))) 1) 2)
(ut ((fn (() 99) ((a (b 1)) (+ a b))) 1 2) 3)
; optional values + variadic parameter
(ut ((fn (((a 1) . xs) (+ a (length xs))))) 1)
(ut ((fn (((a 1) . xs) (+ a (length xs)))) 2) 2)
(ut ((fn (((a 1) . xs) (+ a (length xs)))) 2 3 4) 4)
(ut ((fn (((a 1) (b 2) . xs) (+ a b (length xs))))) 3)
(ut ((fn (((a 1) (b 2) . xs) (+ a b (length xs)))) 2) 4)
(ut ((fn (((a 1) (b 2) . xs) (+ a b (length xs)))) 2 3) 5)
(ut ((fn (((a 1) (b 2) . xs) (+ a b (length xs)))) 2 3 4 5 6) 8)
(ut ((fn (() 99) ((a (b 1) . xs) (+ a b (length xs))))) 99)
(ut ((fn (() 99) ((a (b 1) . xs) (+ a b (length xs)))) 1) 2)
(ut ((fn (() 99) ((a (b 1) . xs) (+ a b (length xs)))) 1 2) 3)
(ut ((fn (() 99) ((a (b 1) . xs) (+ a b (length xs)))) 1 2 3 4 5) 6)


; 1-ary
(ut ((lambda () 12)) 12)
(ut ((lambda (a) a) 1) 1)
(ut ((lambda (a b) (+ a b)) 1 2) 3)
; variadics
(ut ((lambda (x . xs) xs) 1) '())
(ut ((lambda (x . xs) xs) 1 2) '(2))
(ut ((lambda (x . xs) xs) 1 2 3) '(2 3))
; optional arguments
(ut ((lambda ((a 1)) a)) 1)
(ut ((lambda ((a 1)) a) 2) 2)
(ut ((lambda ((a 1) (b 2)) (+ a b))) 3)
(ut ((lambda ((a 1) (b 2)) (+ a b)) 2) 4)
(ut ((lambda ((a 1) (b 2)) (+ a b)) 2 3) 5)
(ut ((lambda (a (b 1)) (+ a b)) 1) 2)
(ut ((lambda (a (b 1)) (+ a b)) 1 2) 3)


(ut (if #t 0 1) 0)
(ut (if #f 0 1) 1)
(ut (if #t 0) 0)
(ut (if #f 0) #void)


(ut (begin) #void)
(ut (begin 1) 1)
(ut (begin 1 2) 2)


(define val 42)
(ut val 42)
(ut (begin (set! val 314) val) 314)


(define val 42)
(ut val 42)
(ut (begin (define val 314) val) 314)
; 1-ary
(ut (begin (define (val) 12) (val)) 12)
(ut (begin (define (val a) a) (val 1)) 1)
(ut (begin (define (val a b) (+ a b)) (val 1 2)) 3)
; variadics
(ut (begin (define (val x . xs) xs) (val 1)) '())
(ut (begin (define (val x . xs) xs) (val 1 2)) '(2))
(ut (begin (define (val x . xs) xs) (val 1 2 3)) '(2 3))
; optional arguments
(ut (begin (define (val (a 1)) a) (val)) 1)
(ut (begin (define (val (a 1)) a) (val 2)) 2)
(ut (begin (define (val (a 1) (b 2)) (+ a b)) (val)) 3)
(ut (begin (define (val (a 1) (b 2)) (+ a b)) (val 2)) 4)
(ut (begin (define (val (a 1) (b 2)) (+ a b)) (val 2 3)) 5)
(ut (begin (define (val a (b 1)) (+ a b)) (val 1)) 2)
(ut (begin (define (val a (b 1)) (+ a b)) (val 1 2)) 3)


(def val 42)
(ut val 42)
(ut (begin (def val 314) val) 314)
; 1-ary
(ut (begin (def (val) 12) (val)) 12)
(ut (begin (def (val a) a) (val 1)) 1)
(ut (begin (def (val a b) (+ a b)) (val 1 2)) 3)
; variadics
(ut (begin (def (val x . xs) xs) (val 1)) '())
(ut (begin (def (val x . xs) xs) (val 1 2)) '(2))
(ut (begin (def (val x . xs) xs) (val 1 2 3)) '(2 3))
; optional arguments
(ut (begin (def (val (a 1)) a) (val)) 1)
(ut (begin (def (val (a 1)) a) (val 2)) 2)
(ut (begin (def (val (a 1) (b 2)) (+ a b)) (val)) 3)
(ut (begin (def (val (a 1) (b 2)) (+ a b)) (val 2)) 4)
(ut (begin (def (val (a 1) (b 2)) (+ a b)) (val 2 3)) 5)
(ut (begin (def (val a (b 1)) (+ a b)) (val 1)) 2)
(ut (begin (def (val a (b 1)) (+ a b)) (val 1 2)) 3)


(ut (defined? acb) #f)
(ut (defined? val) #t)
(ut (defined? defined?) #t)
(ut (defined? def?) #t)


(ut (def? acb) #f)
(ut (def? val) #t)
(ut (def? defined?) #t)
(ut (def? def?) #t)


; arity dispatch 
(ut (begin (defn val ((x . xs) xs)) (val 1)) '())
(ut (begin (defn val ((x . xs) xs)) (val 1 2)) '(2))
(ut (begin (defn val ((x . xs) xs)) (val 1 2 3)) '(2 3))
(ut (begin (defn val (() 1) ((a) a) (xs xs)) (val)) 1)
(ut (begin (defn val (() 1) ((a) a) (xs xs)) (val 2)) 2)
(ut (begin (defn val (() 1) ((a) a) (xs xs)) (val 2 3)) '(2 3))
; optional values
(ut (begin (defn val (((a 1)) a)) (val)) 1)
(ut (begin (defn val (((a 1)) a)) (val 2)) 2)
(ut (begin (defn val (((a 1) (b 2)) (+ a b))) (val)) 3)
(ut (begin (defn val (((a 1) (b 2)) (+ a b))) (val 2)) 4)
(ut (begin (defn val (((a 1) (b 2)) (+ a b))) (val 2 3)) 5)
(ut (begin (defn val (((a 1) (b 2)) (+ a b)) ((a b c) (+ a b c))) (val 2 3 4)) 9)
(ut (begin (defn val (() 99) ((a (b 1)) (+ a b))) (val)) 99)
(ut (begin (defn val (() 99) ((a (b 1)) (+ a b))) (val 1)) 2)
(ut (begin (defn val (() 99) ((a (b 1)) (+ a b))) (val 1 2)) 3)


(ut (and) #t)
(ut (and 1) 1)
(ut (and 1 2) 2)
(ut (and 1 2 3) 3)
(ut (and #f 2 3) #f)
(ut (and 1 #f 3) #f)
(ut (and 1 2 #f) #f)
(ut (and #f #f 3) #f)
(ut (and #f 2 #f) #f)
(ut (and 1 #f #f) #f)
(ut (and #f #f #f) #f)


(ut (or) #f)
(ut (or 1) 1)
(ut (or 1 2) 1)
(ut (or 1 2 3) 1)
(ut (or #f 2 3) 2)
(ut (or 1 #f 3) 1)
(ut (or 1 2 #f) 1)
(ut (or #f #f 3) 3)
(ut (or #f 2 #f) 2)
(ut (or 1 #f #f) 1)
(ut (or #f #f #f) #f)


(define val 42)
(ut (begin (delay (set! val (+ val 1))) val) 42)
(ut (begin (force (delay (set! val (+ val 1)))) val) 43)


(ut (cond) #void)
(ut (cond (#t 1)) 1)
(ut (cond (#f 1)) #void)
(ut (cond (#f 1) (else 2)) 2)
(ut (cond (#t 1) (else 2)) 1)
(ut (cond (#t => boolean?) (else 2)) #t)
(ut (cond (#f => boolean?) (else 2)) 2)
(ut (cond (1 => boolean?) (else 2)) #f)
(ut (cond (#f 0) (#t => boolean?) (#f 0) (else 2)) #t)
(ut (cond (#f 0) (#f => boolean?) (#f 0) (else 2)) 2)
(ut (cond (#f 0) (1 => boolean?) (#f 0) (else 2)) #f)
(ut (cond (#t 0) (#t => boolean?) (#f 0) (else 2)) 0)
(ut (cond (#t 0) (#f => boolean?) (#f 0) (else 2)) 0)
(ut (cond (#t 0) (1 => boolean?) (#f 0) (else 2)) 0)


(define a 100)
(define val 42)
(ut (let ()) #void)
(ut (let () 42) 42)
(ut (let ((a 42)) 314) 314)
(ut (let ((a 42)) a) 42)
(ut (let ((a 42) (b 1)) (+ a b)) 43)
(ut (let ((a 42) (b a)) (+ a b)) 142)
(ut (let func () (if (positive? val) (begin (set! val (- val 1)) (func)) val)) 0)
(ut (let func ((n 5)) (if (< n 2) 1 (* n (func (- n 1))))) 120)
(ut (let func ((n 5) (p 1)) (if (< n 2) p (func (- n 1) (* n p)))) 120)
(ut a 100) ; all `a` changes in `let` were local
(ut val 0) ; verify `val` was properly mutated by named-let
(ut (defined? func) #f) ; all `func` definitions in `let` were local


(ut (quasiquote ()) '())
(ut (quasiquote 1) 1)
(ut (quasiquote (1 2)) (list 1 2))
(ut (quasiquote (1 . b)) (cons 1 'b))
(ut (quasiquote [1 b]) (vector 1 'b))
(ut (quasiquote {1 b}) (hashmap 1 'b))
(ut (quasiquote (+ 1 2)) '(+ 1 2))
; unquote value
(ut (quasiquote ,(+ 1 2)) 3)
(ut (quasiquote (,(+ 1 2))) '(3))
(ut (quasiquote (1 ,(+ 1 2) 5)) '(1 3 5))
(ut (quasiquote (,(+ 0 1) ,(+ 1 2) ,(+ 1 4))) '(1 3 5))
; unquote list
(ut (quasiquote ,(list 1 2)) '(1 2))
(ut (quasiquote (,(list 1 2))) '((1 2)))
(ut (quasiquote (1 ,(list 1 2) 5)) '(1 (1 2) 5))
(ut (quasiquote (,(list 0 1) ,(list 1 2) ,(list 1 4))) '((0 1) (1 2) (1 4)))
; unquote-splice list
(ut (quasiquote (,@(list 1 2))) '(1 2))
(ut (quasiquote (,@(cons 1 2))) '(1 . 2))
(ut (quasiquote (1 ,@(list 1 2) 5)) '(1 1 2 5))
(ut (quasiquote (,@(list 0 1) ,@(list 1 2) ,@(list 1 4))) '(0 1 1 2 1 4))
(ut (quasiquote (,@(list 0 1) ,@(list 1 2) ,@(cons 1 4))) '(0 1 1 2 1 . 4))
; nested quasiquote
(ut `(`,(+ 1 2)) '((quasiquote (unquote (+ 1 2)))))
(ut `(`,,(+ 1 2)) '((quasiquote (unquote 3))))
(ut `(`,@(list 1 2)) '((quasiquote (unquote-splicing (list 1 2)))))
(ut `(`,@(list ,@(list 1 2))) '((quasiquote (unquote-splicing (list 1 2)))))
(ut `,(+ 1 2) 3)
(ut ``,(+ 1 2) '(quasiquote (unquote (+ 1 2))))
(ut ```,(+ 1 2) '(quasiquote (quasiquote (unquote (+ 1 2)))))
(ut ``,,(+ 1 2) '(quasiquote (unquote 3)))
(ut ```,,(+ 1 2) '(quasiquote (quasiquote (unquote (unquote (+ 1 2))))))
; vectors
(ut `[] [])
(ut `[1 (+ 1 1) 3] [1 '(+ 1 1) 3])
(ut `[(+ 0 1) (+ 1 1) (+ 1 2)] ['(+ 0 1) '(+ 1 1) '(+ 1 2)])
(ut `[1 ,(+ 1 1) 3] [1 2 3])
(ut `[,(+ 0 1) ,(+ 1 1) ,(+ 1 2)] [1 2 3])
(ut `([1 (+ 1 1) 3]) (list [1 '(+ 1 1) 3]))
(ut `([(+ 0 1) (+ 1 1) (+ 1 2)]) (list ['(+ 0 1) '(+ 1 1) '(+ 1 2)]))
(ut `([1 ,(+ 1 1) 3]) (list [1 2 3]))
(ut `([,(+ 0 1) ,(+ 1 1) ,(+ 1 2)]) (list [1 2 3]))
; hashmaps
(ut `{} {})
(ut `{a 1 b (+ 1 1) c 3} {'a 1 'b '(+ 1 1) 'c 3})
(ut `{a (+ 0 1) b (+ 1 1) c (+ 1 2)} {'a '(+ 0 1) 'b '(+ 1 1) 'c '(+ 1 2)})
(ut `{a 1 b ,(+ 1 1) c 3} {'a 1 'b 2 'c 3})
(ut `{a ,(+ 0 1) b ,(+ 1 1) c ,(+ 1 2)} {'a 1 'b 2 'c 3})
(ut `({a 1 b (+ 1 1) c 3}) (list {'a 1 'b '(+ 1 1) 'c 3}))
(ut `({a (+ 0 1) b (+ 1 1) c (+ 1 2)}) (list {'a '(+ 0 1) 'b '(+ 1 1) 'c '(+ 1 2)}))
(ut `({a 1 b ,(+ 1 1) c 3}) (list {'a 1 'b 2 'c 3}))
(ut `({a ,(+ 0 1) b ,(+ 1 1) c ,(+ 1 2)}) (list {'a 1 'b 2 'c 3}))
(ut `{(append "a" "a") 1 (append "b" "b") (+ 1 1) (append "c" "c") 3} {'(append "a" "a") 1 '(append "b" "b") '(+ 1 1) '(append "c" "c") 3})
(ut `{(append "a" "a") (+ 0 1) (append "b" "b") (+ 1 1) (append "c" "c") (+ 1 2)} {'(append "a" "a") '(+ 0 1) '(append "b" "b") '(+ 1 1) '(append "c" "c") '(+ 1 2)})
(ut `{(append "a" "a") 1 (append "b" "b") ,(+ 1 1) (append "c" "c") 3} {'(append "a" "a") 1 '(append "b" "b") 2 '(append "c" "c") 3})
(ut `{(append "a" "a") ,(+ 0 1) (append "b" "b") ,(+ 1 1) (append "c" "c") ,(+ 1 2)} {'(append "a" "a") 1 '(append "b" "b") 2 '(append "c" "c") 3})
(ut `{,(append "a" "a") 1 ,(append "b" "b") (+ 1 1) ,(append "c" "c") 3} {"aa" 1 "bb" '(+ 1 1) "cc" 3})
(ut `{,(append "a" "a") (+ 0 1) ,(append "b" "b") (+ 1 1) ,(append "c" "c") (+ 1 2)} {"aa" '(+ 0 1) "bb" '(+ 1 1) "cc" '(+ 1 2)})
(ut `{,(append "a" "a") 1 ,(append "b" "b") ,(+ 1 1) ,(append "c" "c") 3} {"aa" 1 "bb" 2 "cc" 3})
(ut `{,(append "a" "a") ,(+ 0 1) ,(append "b" "b") ,(+ 1 1) ,(append "c" "c") ,(+ 1 2)} {"aa" 1 "bb" 2 "cc" 3})
(ut `({,(append "a" "a") 1 ,(append "b" "b") (+ 1 1) ,(append "c" "c") 3}) (list {"aa" 1 "bb" '(+ 1 1) "cc" 3}))
(ut `({,(append "a" "a") (+ 0 1) ,(append "b" "b") (+ 1 1) ,(append "c" "c") (+ 1 2)}) (list {"aa" '(+ 0 1) "bb" '(+ 1 1) "cc" '(+ 1 2)}))
(ut `({,(append "a" "a") 1 ,(append "b" "b") ,(+ 1 1) ,(append "c" "c") 3}) (list {"aa" 1 "bb" 2 "cc" 3}))
(ut `({,(append "a" "a") ,(+ 0 1) ,(append "b" "b") ,(+ 1 1) ,(append "c" "c") ,(+ 1 2)}) (list {"aa" 1 "bb" 2 "cc" 3}))
; Nesting
(ut `([{}]) '([{}]))
(ut `(a [b {c d} e] f) '(a [b {c d} e] f))
(ut `(a {b [c (d) e] f g} h) '(a {b [c (d) e] f g} h))


(ut (case 42) #void)
(ut (case 42 ((42) 1)) 1)
(ut (case 42 ((1 42 2) 1)) 1)
(ut (case 42 (() 1)) #void)
(ut (case 42 ((314) 1)) #void)
(ut (case 42 (() 1) (else 2)) 2)
(ut (case 42 ((42) 1) (else 2)) 1)
(ut (case 42 ((1 42 2) 1) (else 2)) 1)
(ut (case 42 ((42) => id) (else 2)) '(42))
(ut (case 42 ((1 42 2) => id) (else 2)) '(42 2))
(ut (case 42 (() => id) (else 2)) 2)
(ut (case 42 (() 0) ((42) => id) (() 0) (else 2)) '(42))
(ut (case 42 (() 0) ((1 42 2) => id) (() 0) (else 2)) '(42 2))
(ut (case 42 (() 0) (() => id) (() 0) (else 2)) 2)
(ut (case 42 ((42) 0) ((42) => id) (() 0) (else 2)) 0)
(ut (case 42 ((1 42 2) 0) ((1 42 2) => id) (() 0) (else 2)) 0)
(ut (case 42 ((42) 0) (() => id) (() 0) (else 2)) 0)
(ut (case 42 ((1 42 2) 0) (() => id) (() 0) (else 2)) 0)


(define a 100)
(ut (let* ()) #void)
(ut (let* () 42) 42)
(ut (let* ((a 42)) 314) 314)
(ut (let* ((a 42)) a) 42)
(ut (let* ((a 42) (b 1)) (+ a b)) 43)
(ut (let* ((a 42) (b a)) (+ a b)) 84)
(ut a 100) ; all `a` changes in `let` were local


(ut (letrec () 42) 42)
(ut (letrec ((a (lambda (n) (if (< n 2) 1 (* n (a (- n 1))))))) (a 5)) 120)
(ut (letrec ((a (lambda (n) (if (< n 2) 1 (* n (a (- n 1))))))
             (b (lambda (n) (if (< n 2) 1 (* n (b (- n 1)))))) ) (a (b 3))) 720)
(ut a 100) ; all `a` changes in `letrec` were local
(ut
  (letrec ((is-even? (lambda (n) (or (zero? n) (is-odd? (- n 1)))))
           (is-odd? (lambda (n) (and (positive? n) (is-even? (- n 1))))))
    (is-odd? 11))
  #t)


(ut (letrec* () 42) 42)
(ut (letrec* ((a (lambda (n) (if (< n 2) 1 (* n (a (- n 1))))))) (a 5)) 120)
(ut (letrec* ((a (lambda (n) (if (< n 2) 1 (* n (a (- n 1))))))
              (b (lambda (n) (if (< n 2) 1 (* n (b (- n 1))))))) (a (b 3))) 720)
(ut (letrec* ((a (lambda (n) (if (< n 2) 1 (* n (a (- n 1)))))) (b a)) (a (b 3))) 720)
(ut a 100) ; all `a` changes in `letrec` were local


(ut (unless #t 1) #void)
(ut (unless #f 1) 1)
(ut (unless #t 1 2) #void)
(ut (unless #f 1 2) 2)

(ut (when #t 1) 1)
(ut (when #f 1) #void)
(ut (when #t 1 2) 2)
(ut (when #f 1 2) #void)


(define val 0)
(call/cc
  (lambda (exit)
    (while (#t)
      (if (< val 10) 
          (set! val (+ val 1))
          (exit val))))) ; exit out of the infinite loop
(ut val 10)
(define val 0)
(ut (while ((< val 10) val) (set! val (+ val 1))) 10)


(define val 0)
(call/cc
  (lambda (exit)
    (do ()
        ()
      (if (< val 10) 
          (set! val (+ val 1))
          (exit val))))) ; exit out of the infinite loop
(ut val 10)
(ut (do ((val 0 (+ val 1))) ((= val 10) val)) 10)
(ut (do ((i 0 (+ i 1)) (j 1 (* j 2))) ((= i 10) j) (set! i (+ i 1))) 32)


(ut (-<> 42) 42)
(ut (-<> 42 314) 314)
(ut (-<> 2 (* <> <>) (/ <> 3)) 4/3)


(ut ((curry () 1)) 1)
(ut ((curry (a) a) 1) 1)
(ut (procedure? ((curry (a b) a) 1)) #t)
(ut ((curry (a b) a) 1 2) 1)
(ut (((curry (a b) a) 1) 2) 1)


(define (vals0) (values))
(define (vals1) (values 1))
(define (vals2) (values 1 2))
(ut (let-values () 42) 42)
(ut (let-values ((() (vals0))) 42) 42)
(ut (let-values (((a) (vals1))) a) 1)
(ut (let-values (((a b) (vals2))) (+ a b)) 3)
(ut (let-values (((a) (vals1)) ((b c) (vals2))) (+ a b c)) 4)


(define (val x) (raise 'an-error) x)
(define (val2 x) (raise "an-error") x)
(define (val3 x)  (raise :an-error) x)
(define (val4 x) x)
(ut
  (guard 
    (err
     ((symbol? err) 0)
     ((string? err) 1)
     (else 2))
    (val 42))
  0)
(ut
  (guard 
    (err
     ((symbol? err) 0)
     ((string? err) 1)
     (else 2))
    (val2 42))
  1)
(ut
  (guard 
    (err
     ((symbol? err) 0)
     ((string? err) 1)
     (else 2))
    (val3 42))
  2)
(ut
  (guard 
    (err
     ((symbol? err) 0)
     ((string? err) 1)
     (else 2))
    (val4 42))
  42)
(ut
  (guard 
    (err
     ((symbol? err) 0)
     ((string? err) 1)
     (else 2))
    :ignore
    :ignore
    (val 42))
  0)


(define val2 0)
(ut (parameter? val2) #f)
(define-parameter val2 10)
(ut (parameter? val2) #t)
(ut (get-parameter val2) 10)
(set-parameter! val2 11)
(ut val2 0)
(ut (get-parameter val2) 11)

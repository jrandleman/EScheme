;; Author: Jordan Randleman -- specials.scm
;; => Tests for EScheme's primitive special forms.
;;    Invoked by ../main.scm
;; => Realistically most of these working predicate unit tests executing at
;;    all, but we still fully exercise the forms to validate corner cases.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." "lib.scm"))


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


(define val2 0)
(ut (parameter? val2) #f)
(define-parameter val2 10)
(ut (parameter? val2) #t)


(ut (-<> 42) 42)
(ut (-<> 42 314) 314)
(ut (-<> 2 (* <> <>) (/ <> 3)) 4/3)


(ut ((curry () 1)) 1)
(ut ((curry (a) a) 1) 1)
(ut (procedure? ((curry (a b) a) 1)) #t)
(ut ((curry (a b) a) 1 2) 1)
(ut (((curry (a b) a) 1) 2) 1)


; <dosync> (etc.) correctness across threads is checked by "./procedures/concurrent.scm"
(define module-dir (path #path ".." "examples" "specials"))
(import module-dir module1)
(ut (syntax? dosync) #t)
(ut (syntax? dosync-module) #t)
(ut (syntax? dosync-with) #t)
(ut (eq? module1.*dosync-module-lock* *dosync-module-lock*) #f) ; module-specific (non-parameter)
(ut (eq? module1.*dosync-lock* *dosync-lock*) #t) ; module-global (parameter)


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


(ut (thread-defined? def) #f)
(ut (thread-defined? abc) #f)
(thread-define abc 0)
(ut (defined? abc) #f)
(ut (thread-defined? abc) #t)
(ut (thread-get abc) 0)
(ut (begin (thread-set! abc 1) (thread-get abc)) 1)
(define c-t (current-thread))
(ut (thread-defined? c-t def) #f)
(ut (thread-defined? c-t abc) #t) ; true b/c in meta-thread
(thread-define c-t abc 0)
(ut (defined? abc) #f)
(ut (thread-defined? c-t abc) #t)
(ut (thread-get c-t abc) 0)
(ut (begin (thread-set! c-t abc 2) (thread-get c-t abc)) 2)
(ut (thread-get abc) 1) ; validate didn't change in meta-thread
(define t 
  (thread 
    (lambda ()
      (define c-t (current-thread))
      (ut (thread-defined? c-t def) #f)
      (ut (thread-defined? c-t abc) #t) ; true b/c in meta-thread
      (thread-define c-t abc 0)
      (ut (defined? abc) #f)
      (ut (thread-defined? c-t abc) #t)
      (ut (thread-get c-t abc) 0)
      (ut (begin (thread-set! c-t abc 3) (thread-get c-t abc)) 3)
      (ut (thread-get abc) 1)))) ; validate didn't change in meta-thread
(thread-start! t)
(thread-join! t)
(ut (thread-get c-t abc) 2) ; validate didn't change in current-thread


; helper syntax to reuse c****r tests from './procedures/pair.scm' as sc****r test
(define-syntax stream
  (lambda xs
    (if (null? xs) '() `(scons ,(car xs) (stream ,@(cdr xs))))))

(define s1 (scons 0 (scons 1 (scons 2 (scons 3 '())))))
(ut (stream-pair? s1) #t)
(ut (stream-pair? '()) #f)
(ut (stream? s1) #t)
(ut (stream? '()) #t)

(ut (scaar (stream (stream 1))) 1)
(ut (scadr (stream 1 2)) 2)
(ut (scar (scdar (stream (stream 1 2)))) 2)
(ut (scar (scddr (stream 1 2 3))) 3)

(ut (scaaar (stream (stream (stream 1)))) 1)
(ut (scaadr (stream 1 (stream 2))) 2)
(ut (scar (scadar (stream (stream 1 (stream 2))))) 2)
(ut (scar (scaddr (stream 1 2 (stream 3)))) 3)
(ut (scar (scdaar (stream (stream (stream 1 2))))) 2)
(ut (scar (scdadr (stream 1 (stream 2 3)))) 3)
(ut (scar (scddar (stream (stream 1 2 3)))) 3)
(ut (scar (scdddr (stream 1 2 3 4))) 4)

(ut (scaaaar (stream (stream (stream (stream 1))))) 1)
(ut (scaaadr (stream 1 (stream (stream 2)))) 2)
(ut (scar (scaadar (stream (stream 1 (stream (stream 2)))))) 2)
(ut (scar (scaaddr (stream 1 2 (stream (stream 3))))) 3)
(ut (scar (scadaar (stream (stream (stream 1 (stream 2)))))) 2)
(ut (scar (scadadr (stream 1 (stream 2 (stream 3))))) 3)
(ut (scar (scaddar (stream (stream 1 2 (stream 3))))) 3)
(ut (scar (scadddr (stream 1 2 3 (stream 4)))) 4)
(ut (scar (scdaaar (stream (stream (stream (stream 1 2)))))) 2)
(ut (scar (scdaadr (stream 1 (stream (stream 2 3))))) 3)
(ut (scar (scdadar (stream (stream 1 (stream 2 3))))) 3)
(ut (scar (scdaddr (stream 1 2 (stream 3 4)))) 4)
(ut (scar (scddaar (stream (stream (stream 1 2 3))))) 3)
(ut (scar (scddadr (stream 1 (stream 2 3 4)))) 4)
(ut (scar (scdddar (stream (stream 1 2 3 4)))) 4)
(ut (scar (scddddr (stream 1 2 3 4 5))) 5)

(define pos-ints (let loop ((n 1)) (scons n (loop (+ n 1)))))
(ut (stream->list pos-ints 0) '())
(ut (stream->list pos-ints 10) '(1 2 3 4 5 6 7 8 9 10))
(ut (stream-ref pos-ints 0) 1)
(ut (stream-ref pos-ints 9) 10)

(define even-pos-ints (stream-map + pos-ints pos-ints))
(ut (stream->list even-pos-ints 0) '())
(ut (stream->list even-pos-ints 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints 0) 2)
(ut (stream-ref even-pos-ints 9) 20)

(define neg-ints (stream-map - pos-ints))
(ut (stream->list neg-ints 0) '())
(ut (stream->list neg-ints 10) '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
(ut (stream-ref neg-ints 0) -1)
(ut (stream-ref neg-ints 9) -10)

(define even-pos-ints-2 (stream-filter even? pos-ints))
(ut (stream->list even-pos-ints-2 0) '())
(ut (stream->list even-pos-ints-2 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints-2 0) 2)
(ut (stream-ref even-pos-ints-2 9) 20)

(define even-pos-ints-3 (stream-iterate (lambda (x) (+ x 2)) 2))
(ut (stream->list even-pos-ints-3 0) '())
(ut (stream->list even-pos-ints-3 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints-3 0) 2)
(ut (stream-ref even-pos-ints-3 9) 20)

(define stream-of-1-2-3s (stream-constant 1 2 3))
(ut (stream->list stream-of-1-2-3s 0) '())
(ut (stream->list stream-of-1-2-3s 10) '(1 2 3 1 2 3 1 2 3 1))
(ut (stream-ref stream-of-1-2-3s 0) 1)
(ut (stream-ref stream-of-1-2-3s 9) 1)
(ut (stream-constant) '())

(define stream-of-1-2-3 (stream-append (stream 1) (stream 2) (stream 3)))
(ut (stream->list stream-of-1-2-3 0) '())
(ut (stream->list stream-of-1-2-3 3) '(1 2 3))
(ut (stream-ref stream-of-1-2-3 0) 1)
(ut (stream-ref stream-of-1-2-3 2) 3)

(define odd-pos-ints (stream-filter odd? pos-ints))
(define pos-ints-2 (stream-interleave odd-pos-ints even-pos-ints))
(ut (stream->list pos-ints-2 0) '())
(ut (stream->list pos-ints-2 10) '(1 2 3 4 5 6 7 8 9 10))
(ut (stream-ref pos-ints-2 0) 1)
(ut (stream-ref pos-ints-2 9) 10)

(define even-pos-ints-generator (stream->generator even-pos-ints))
(do ((i 0 (+ i 1)))
    ((= i 10))
    (ut (even-pos-ints-generator) (stream-ref even-pos-ints i)))


(define generator-container [])
(define-generator (save-numbers start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container i)
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))
(define-generator (save-strings start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container (number->string i))
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))
(complete-all-generators! (save-numbers 0 10) (save-strings 0 10))
(ut generator-container [0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9"])

(set! generator-container [])
(define-generator (save-chars start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container (integer->char i))
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))

(complete-n-generators! 10 (save-numbers 0 10) (save-strings 0 10) (save-chars 0 100))
(ut (>= (length generator-container) 30) #t)

(define-generator (powers-of-2-generator)
  (let loop ((n 0))
    (yield (expt 2 n))
    (loop (+ n 1))))
(define powers-of-2 (powers-of-2-generator))
(ut (powers-of-2) 1)
(ut (powers-of-2) 2)
(ut (powers-of-2) 4)
(ut (powers-of-2) 8)
(ut (powers-of-2) 16)
(ut (powers-of-2) 32)


; We just check macro syntax here, semantic correctness tested by './procedures/oo.scm'
(ut (class? (class)) #t)
(ut (class? (class (:extends (class)))) #t)
(ut (class? (class (:extends (class)) (field 0))) #t)
(ut (class? (class (field 0))) #t)
(ut (class? (class (:extends (class)) ((method) 0))) #t)
(ut (class? (class ((method) 0))) #t)
(ut (class? (class (:extends (class)) (field 0) ((method) 0))) #t)
(ut (class? (class (field 0) ((method) 0))) #t)
(ut (class? (class (:extends (class)) (:static field 0))) #t)
(ut (class? (class (:static field 0))) #t)
(ut (class? (class (:extends (class)) (:static (method) 0))) #t)
(ut (class? (class (:static (method) 0))) #t)
(ut (class? (class (:extends (class)) (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:extends (class)) (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)

(ut (class? (class (:implements (interface) (interface)))) #t)
(ut (class? (class (:implements (interface) (interface)) (field 0))) #t)
(ut (class? (class (field 0))) #t)
(ut (class? (class (:implements (interface) (interface)) ((method) 0))) #t)
(ut (class? (class ((method) 0))) #t)
(ut (class? (class (:implements (interface) (interface)) (field 0) ((method) 0))) #t)
(ut (class? (class (field 0) ((method) 0))) #t)
(ut (class? (class (:implements (interface) (interface)) (:static field 0))) #t)
(ut (class? (class (:static field 0))) #t)
(ut (class? (class (:implements (interface) (interface)) (:static (method) 0))) #t)
(ut (class? (class (:static (method) 0))) #t)
(ut (class? (class (:implements (interface) (interface)) (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:implements (interface) (interface)) (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)

(ut (class? (class (:extends (class)) (:implements (interface) (interface)))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (field 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) ((method) 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (field 0) ((method) 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (:static field 0))) #t)
(ut (class? (class (:static field 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (:static (method) 0))) #t)
(ut (class? (class (:static (method) 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0))) #t)
(ut (class? (class (:extends (class)) (:implements (interface) (interface)) (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)
(ut (class? (class (:static field 0) (:static (method) 0) (field 0) ((method) 0))) #t)

(define-class class-1)
(define-class class-2 (field 0))
(ut (class? class-1) #t)
(ut (class? class-2) #t)
(ut (procedure? class-1?) #t)
(ut (procedure? class-2?) #t)

(ut (interface? (interface (:extends))) #t)
(ut (interface? (interface (:extends (interface) (interface)))) #t)
(ut (interface? (interface (:extends (interface) (interface)) field)) #t)
(ut (interface? (interface field)) #t)
(ut (interface? (interface (:extends (interface) (interface)) method)) #t)
(ut (interface? (interface method)) #t)
(ut (interface? (interface (:extends (interface) (interface)) field method)) #t)
(ut (interface? (interface field method)) #t)
(ut (interface? (interface (:extends (interface) (interface)) (:static field 0))) #t)
(ut (interface? (interface (:static field 0))) #t)
(ut (interface? (interface (:extends (interface) (interface)) (:static (method) 0))) #t)
(ut (interface? (interface (:static (method) 0))) #t)
(ut (interface? (interface (:extends (interface) (interface)) (:static field 0) (:static (method) 0))) #t)
(ut (interface? (interface (:static field 0) (:static (method) 0))) #t)
(ut (interface? (interface (:extends (interface) (interface)) (:static field 0) (:static (method) 0) field method)) #t)
(ut (interface? (interface (:static field 0) (:static (method) 0) field method)) #t)

(define-interface interface-1)
(define-interface interface-2 field)
(ut (interface? interface-1) #t)
(ut (interface? interface-2) #t)
(ut (procedure? interface-1?) #t)
(ut (procedure? interface-2?) #t)

(define-interface Interface1)
(define-class BaseClass1
  ((new x) 
    (define self.x x)))
(define-class SuperClass1 (:extends BaseClass1) (:implements Interface1)
  ((new x) 
    (super! x) ; show `super!` works
    (set! self.x (* self.x self.x)) ; show dot-notation works with `set!`
    (define self.y (* self.x self.x)))) ; show dot-notation works with `define`

(define sc (SuperClass1 2))
(ut sc.x 4)
(ut sc.y 16)
(ut (defined? sc.x) #t) ; show dot-notation works with `defined?`
(ut (defined? sc.y) #t)
(ut (defined? sc.a) #f)
(ut (defined? SuperClass1.name) #t)
(ut (defined? Interface1.name) #t)
(ut (defined? sc.class) #t)

(define-class BaseClass2
  ((new x y z) 
    (define self.x (+ x y z))))
(define-class SuperClass2 (:extends BaseClass2)
  ((new x) 
    (apply-super! (list x x x)))) ; show `apply-super!` works

(define sc2 (SuperClass2 2))
(ut sc2.x 6)


(import module-dir module2)
(ut module2.v [])
(define v module2.v)
(import module-dir module2)
(ut (eq? v module2.v) #t) ; show modules cache
(reload module2)
(ut (eq? v module2.v) #f) ; show reloaded modules
(ut (begin (define module2.v2 88) module2.v2) 88) ; show <define> on modules
(ut (begin (set! module2.v2 99) module2.v2) 99) ; show <set!> on modules
(ut (defined? module2.v) #t) ; show <defined?> on modules
(ut (defined? module2.v3) #f) ; show <defined?> on modules

(import module-dir module3 :as m3)
(ut m3.v [])
(define v m3.v)
(import module-dir module3 :as m3)
(ut (eq? v m3.v) #t) ; show modules cache
(reload m3)
(ut (eq? v m3.v) #f) ; show reloaded modules

(import examples.specials.module4)
(ut module4.v [])
(define v module4.v)
(import examples.specials.module4)
(ut (eq? v module4.v) #t) ; show modules cache
(reload module4)
(ut (eq? v module4.v) #f) ; show reloaded modules

(import examples.specials.module5 :as m5)
(ut m5.v [])
(define v m5.v)
(import examples.specials.module5 :as m5)
(ut (eq? v m5.v) #t) ; show modules cache
(reload m5)
(ut (eq? v m5.v) #f) ; show reloaded modules

(from module-dir module6 :import v)
(ut v 0)
(ut (defined? module6) #f)

(from module-dir module7 :import v :as v-alias)
(ut v-alias 1)
(ut (defined? module7) #f)

(from examples.specials.module8 :import v)
(ut v 2)
(ut (defined? module8) #f)

(from examples.specials.module9 :import v :as v-alias)
(ut v-alias 3)
(ut (defined? module9) #f)


(define (vals0) (values))
(define (vals1) (values 1))
(define (vals2) (values 1 2))
(ut (let-values () 42) 42)
(ut (let-values ((() (vals0))) 42) 42)
(ut (let-values (((a) (vals1))) a) 1)
(ut (let-values (((a b) (vals2))) (+ a b)) 3)
(ut (let-values (((a) (vals1)) ((b c) (vals2))) (+ a b c)) 4)

;; Author: Jordan Randleman -- meta.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define data-directory (append (path (path-parent #path 2) "examples" "procedures" "system") *file-separator*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(import data-directory module-test)

(ut (apply + (list 1 2 3)) 6)

(ut (eval-bytecode (compile '(if 1 2 3))) 2)
(ut (eval '(if 1 2 3)) 2)

(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym) (gensym)) #f)
(ut (equal? (gensym 'abc) (gensym 'abc)) #f)

(ut (syntax? 0) #f)
(ut (syntax? syntax?) #f)
(ut (syntax? if) #t)
(ut (syntax? module-test.f) #f) ; verify works with modules
(ut (syntax? module-test.*3) #t) ; verify works with modules

(ut (list? (expand-syntax '(if 1 2 3))) #t)
(ut (expand-syntax '(module-test.*3 5)) '(* 5 3)) ; verify works with modules

(ut (callable-name (begin (define (f x) x) f)) 'f) ; dynamic lambda
(ut (callable-name (begin (define :int (f :int x) x) f)) 'f) ; typed lambda
(ut (callable-name (begin (defn f ((x) x) ((y) y)) f)) 'f) ; dynamic fn
(ut (callable-name (begin (defn f (:int (:int x) x) (:char (:char y) y)) f)) 'f) ; typed fn
(ut (callable-name (begin (defclass C) C)) 'C) ; constructor
(ut (callable-name callable-name) 'callable-name) ; primitive

; can't verify these reflections programmatically, just check we can call it.
(ut (pair? (callable-signature callable-signature)) #t) 
(ut (string? (docstring docstring)) #t)

(define-type int-or-char :int|char)

(ut ((fn ((:int-or-char x) #t) ((x) #f)) 42) #t)
(ut ((fn ((:int-or-char x) #t) ((x) #f)) #\a) #t)
(ut ((fn ((:int-or-char x) #t) ((x) #f)) "a") #f)

(ut (type-alias? 42) #f)
(ut (type-alias? int-or-char) #t)

(ut (type-alias-source int-or-char) :int|char)

(ut (type? :int-or-char) #t)
(ut (type? :char-or-int) #f)
(ut (type? :int|char) #t)
(ut (type? :ClassNameThatDoesNotExist) #f)
(ut (type? :vec<int|char>) #t)
(ut (type? :vec<ClassNameThatDoesNotExist>) #f)

(ut (type? 42 :int) #t)
(ut (type? 42 :str) #f)
(ut (type? 42 :int-or-char) #t) ; support for passing alias objects too
(ut (type? #\a :int-or-char) #t)
(ut (type? "a" :int-or-char) #f)

(ut (type=? :int :str) #f)
(ut (type=? :int|str :str|int) #t)
(ut (type=? :int :int :int :str :int) #f) ; more than 2 arguments
(ut (type=? :int|str :str|int :str|int :str|int) #t) ; more than 2 arguments
(ut (type=? :int|str|char :str|int) #f)
(ut (type=? :int|str :str|int|char) #f)
(ut (type=? :map<any,any> :map<any,any>) #t)
(ut (type=? :map<any,int> :map<any,int>) #t)
(ut (type=? :map<int,any> :map<int,any>) #t)
(ut (type=? :map<int,any> :map<any,int>) #f)
(ut (type=? :map<any,int> :map<int,any>) #f)
(ut (type=? :vec<any> :vec<any>) #t)
(ut (type=? :vec<any> :vec<int>) #f)
(ut (type=? :vec<int> :vec<any>) #f)
(ut (type=? :vec<int|str>|char :char|vec<str|int>) #t)
(ut (type=? :vec<int|str>|char :char|vec<key|int>) #f)
(ut (type=? :int-or-char|str :str|int|char) #t) ; supports deconstructing aliases
(ut (type=? :int-or-char|str :str|key|char) #f)
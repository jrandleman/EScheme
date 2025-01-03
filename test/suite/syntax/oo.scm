;; Author: Jordan Randleman -- oo.scm
;; => Tests for EScheme's primitive generator syntax.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
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

(ut (interface? (interface (:extends (interface)))) #t)
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

; verify aliases
(eq? defclass define-class)
(eq? definterface define-interface)

; verify <docstring>
(ut (number? (string-contains (docstring (class "Class <docstring> test.")) "Class <docstring> test.")) #t)
(ut (number? (string-contains (docstring (class "Class <docstring> test." (field 1))) "Class <docstring> test.")) #t)
(ut (number? (string-contains (docstring (class (:extends BaseClass1) "Class <docstring> test.")) "Class <docstring> test.")) #t)
(ut (number? (string-contains (docstring (class (:implements Interface1) "Class <docstring> test.")) "Class <docstring> test.")) #t)
(ut (number? (string-contains (docstring (class (:extends BaseClass1) (:implements Interface1) "Class <docstring> test.")) "Class <docstring> test.")) #t)
(ut (number? (string-contains (docstring (class (:extends BaseClass1) (:implements Interface1) "Class <docstring> test." (field 1))) "Class <docstring> test.")) #t)

(define-class DocstringClass "Class <docstring> test.")
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)
(define-class DocstringClass "Class <docstring> test." (field 1))
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)
(define-class DocstringClass (:extends BaseClass1) "Class <docstring> test.")
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)
(define-class DocstringClass (:implements Interface1) "Class <docstring> test.")
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)
(define-class DocstringClass (:extends BaseClass1) (:implements Interface1) "Class <docstring> test.")
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)
(define-class DocstringClass (:extends BaseClass1) (:implements Interface1) "Class <docstring> test." (field 1))
(ut (number? (string-contains (docstring DocstringClass) "Class <docstring> test.")) #t)

(ut (number? (string-contains (docstring (interface "Interface <docstring> test.")) "Interface <docstring> test.")) #t)
(ut (number? (string-contains (docstring (interface "Interface <docstring> test." field)) "Interface <docstring> test.")) #t)
(ut (number? (string-contains (docstring (interface (:extends Interface1) "Interface <docstring> test.")) "Interface <docstring> test.")) #t)
(ut (number? (string-contains (docstring (interface (:extends Interface1) "Interface <docstring> test." field)) "Interface <docstring> test.")) #t)

(define-interface DocstringInterface "Interface <docstring> test.")
(ut (number? (string-contains (docstring DocstringInterface) "Interface <docstring> test.")) #t)
(define-interface DocstringInterface "Interface <docstring> test." field)
(ut (number? (string-contains (docstring DocstringInterface) "Interface <docstring> test.")) #t)
(define-interface DocstringInterface (:extends Interface1) "Interface <docstring> test.")
(ut (number? (string-contains (docstring DocstringInterface) "Interface <docstring> test.")) #t)
(define-interface DocstringInterface (:extends Interface1) "Interface <docstring> test." field)
(ut (number? (string-contains (docstring DocstringInterface) "Interface <docstring> test.")) #t)

; verify we support required method signatures in interfaces
(define-interface InterfaceWithTypeSignatures 
  ((generic-nullary-method)) ; typeless nullary
  (:num (unary-method :str x)) ; typed unary
  (binary-method ; typed n-ary
    (fn
      (:num (:int x :char y))
      (:int (:vec x :int y)))))
(ut 
  (class? 
    (class (:implements InterfaceWithTypeSignatures) 
      ((generic-nullary-method) #f)
      (:num (unary-method :str x) (len x))
      (binary-method 
        (fn (:num (:int x :char y) x) (:int (:vec x :int y) (len x))))))
  #t)

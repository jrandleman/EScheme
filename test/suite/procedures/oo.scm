;; Author: Jordan Randleman -- oo.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (meta-object? 0) #f)
(ut (meta-object? (class)) #t)
(ut (meta-object? ((class))) #t)
(ut (meta-object? (interface)) #t)

(import (path (path-parent #path 2) "examples" "procedures" "system") module-test)
(ut (meta-object? module-test) #f) ; showing modules can use dot-syntax, but ARE NOT meta-objects

(ut (interface? 0) #f)
(ut (interface? (class)) #f)
(ut (interface? ((class))) #f)
(ut (interface? (interface)) #t)

(ut (class? 0) #f)
(ut (class? (class)) #t)
(ut (class? ((class))) #f)
(ut (class? (interface)) #f)

(ut (object? 0) #f)
(ut (object? (class)) #f)
(ut (object? ((class))) #t)
(ut (object? (interface)) #f)

(ut (functor? 0) #f)
(ut (functor? (class)) #f)
(ut (functor? (class ((->procedure x) x))) #f)
(ut (functor? ((class))) #f)
(ut (functor? ((class ((->procedure x) x)))) #t)
(ut (functor? (interface)) #f)

(define-interface IBase1 IBase1v (:static sIBase1v 'sIBase1v))
(define-interface IBase2 IBase2v (:static sIBase2v 'sIBase2v))
(define-interface I1 (:extends IBase1 IBase2) (:static sI1v 'sI1v) I1v)
(define-interface I2 I2v (:static sI2v 'sI2v))
(define-class CBase (:implements I1 I2) (:static sCBasev 'sCBasev) (IBase1v 'IBase1v) (IBase2v 'IBase2v) (I1v 'I1v) (I2v 'I2v) (CBasev 'CBasev))
(define-class C1 (:extends CBase) (:static sC1v 'sC1v) (C1v 'C1v))
(define-class C2 (:static sC2v 'sC2v) (C2v 'C2v))
(define o1 (C1))
(define o2 (C2))

(ut (oo-is? o1 C1) #t)
(ut (oo-is? o1 CBase) #t)
(ut (oo-is? o1 I1) #t)
(ut (oo-is? o1 I2) #t)
(ut (oo-is? o1 IBase1) #t)
(ut (oo-is? o1 IBase2) #t)
(ut (oo-is? o2 C2) #t)
(ut (oo-is? o2 CBase) #f)
(ut (oo-is? o2 I1) #f)
(ut (oo-is? o2 I2) #f)
(ut (oo-is? o2 IBase1) #f)
(ut (oo-is? o2 IBase2) #f)

; test <oo-has?> on all meta objects
(ut (oo-has? o1 'C1v) #t)
(ut (oo-has? o1 'C2v) #f)
(ut (oo-has? o1 'CBasev) #t)
(ut (oo-has? o1 'I1v) #t)
(ut (oo-has? o1 'I2v) #t)
(ut (oo-has? o1 'IBase1v) #t)
(ut (oo-has? o1 'IBase2v) #t)
(ut (oo-has? o2 'C2v) #t)
(ut (oo-has? o2 'C1v) #f)
(ut (oo-has? o2 'CBasev) #f)
(ut (oo-has? o2 'I1v) #f)
(ut (oo-has? o2 'I2v) #f)
(ut (oo-has? o2 'IBase1v) #f)
(ut (oo-has? o2 'IBase2v) #f)

(ut (oo-has? C1 'sC1v) #t)
(ut (oo-has? C1 'sC2v) #f)
(ut (oo-has? C1 'sCBasev) #t)
(ut (oo-has? C1 'sI1v) #f)
(ut (oo-has? C1 'sI2v) #f)
(ut (oo-has? C1 'sIBase1v) #f)
(ut (oo-has? C1 'sIBase2v) #f)
(ut (oo-has? C2 'sC2v) #t)
(ut (oo-has? C2 'sC1v) #f)
(ut (oo-has? C2 'sCBasev) #f)
(ut (oo-has? C2 'sI1v) #f)
(ut (oo-has? C2 'sI2v) #f)
(ut (oo-has? C2 'sIBase1v) #f)
(ut (oo-has? C2 'sIBase2v) #f)

(ut (oo-has? I1 'sI1v) #t)
(ut (oo-has? I1 'sI2v) #f)
(ut (oo-has? I1 'sIBase1v) #f)
(ut (oo-has? I1 'sIBase2v) #f)

(ut (oo-has? I2 'sI1v) #f)
(ut (oo-has? I2 'sI2v) #t)
(ut (oo-has? I2 'sIBase1v) #f)
(ut (oo-has? I2 'sIBase2v) #f)

; test <oo-has?> nested object-access-chains
(define-class NestedC0 (:static v0 0))
(define-class NestedC1 (:static v1 NestedC0))
(define-class NestedC2 (:static v2 NestedC1))
(define-class NestedC3 (:static v3 NestedC2))

(ut (oo-has? NestedC3 'v3 'v2 'v1 'v0) #t)
(ut (oo-has? NestedC3 'v3 'v2.v1 'v0) #t)
(ut (oo-has? NestedC3 'v3.v2 'v1.v0) #t)
(ut (oo-has? NestedC3 'v3.v2.v1.v0) #t)

; test <oo-get> on all meta objects
(ut (oo-get o1 'C1v) 'C1v)
(ut (oo-get o1 'CBasev) 'CBasev)
(ut (oo-get C1 'sC1v) 'sC1v)
(ut (oo-get C1 'sCBasev) 'sCBasev)
(ut (oo-get C2 'sC2v) 'sC2v)
(ut (oo-get I1 'sI1v) 'sI1v)
(ut (oo-get I2 'sI2v) 'sI2v)

; test <oo-get> nested object-access-chains
(ut (oo-get NestedC3 'v3 'v2 'v1 'v0) 0)
(ut (oo-get NestedC3 'v3 'v2.v1 'v0) 0)
(ut (oo-get NestedC3 'v3.v2 'v1.v0) 0)
(ut (oo-get NestedC3 'v3.v2.v1.v0) 0)

; test <oo-set!> on all meta objects
(ut (begin (oo-set! C1 'sC1v 1) C1.sC1v) 1)
(ut (begin (oo-set! C1 'sCBasev 2) C1.sCBasev) 2)
(ut (begin (oo-set! C2 'sC2v 3) C2.sC2v) 3)
(ut (begin (oo-set! I1 'sI1v 4) I1.sI1v) 4)
(ut (begin (oo-set! I2 'sI2v 5) I2.sI2v) 5)

(ut (begin (oo-set! o1 'C1v 1) o1.C1v) 1)
(ut (begin (oo-set! o1 'CBasev 2) o1.CBasev) 2)
(ut (begin (oo-set! o1 'I1v 4) o1.I1v) 4)
(ut (begin (oo-set! o1 'I2v 5) o1.I2v) 5)

; test <oo-set!> nested object-access-chains
(ut (begin (oo-set! NestedC3 'v3 'v2 'v1 'v0 1) NestedC3.v3.v2.v1.v0) 1)
(ut (begin (oo-set! NestedC3 'v3 'v2.v1 'v0 2) NestedC3.v3.v2.v1.v0) 2)
(ut (begin (oo-set! NestedC3 'v3.v2 'v1.v0 3) NestedC3.v3.v2.v1.v0) 3)
(ut (begin (oo-set! NestedC3 'v3.v2.v1.v0 4) NestedC3.v3.v2.v1.v0) 4)

; test <oo-define> on all meta objects
(ut (begin (oo-define o1 'abc 77) o1.abc) 77)
(ut (begin (oo-define C1 'abc 88) C1.abc) 88)
(ut (begin (oo-define I2 'abc 99) I2.abc) 99)

; test <oo-define> nested object-access-chains
(ut (begin (oo-define NestedC3 'v3 'v2 'v1 'v6 5) NestedC3.v3.v2.v1.v6) 5)
(ut (begin (oo-define NestedC3 'v3 'v2.v1 'v7 6) NestedC3.v3.v2.v1.v7) 6)
(ut (begin (oo-define NestedC3 'v3.v2 'v1.v8 7) NestedC3.v3.v2.v1.v8) 7)
(ut (begin (oo-define NestedC3 'v3.v2.v1.v9 8) NestedC3.v3.v2.v1.v9) 8)

(ut (oo-is? (oo-super o1) CBase) #t)
(ut (oo-super C1) CBase)
(ut (oo-super C2) #f)

(ut (oo-interfaces o1) '())
(ut (oo-interfaces C1) '())
(ut (oo-interfaces I2) '())
(list-has-values? (oo-interfaces (oo-super C1)) I1 I2)
(list-has-values? (oo-interfaces I1) IBase1 IBase2)

(list-has-values? (oo-properties o1) 'I1v 'IBase2v 'class 'I2v 'IBase1v 'C1v 'CBasev 'abc)
(list-has-values? (oo-properties C1) '(:static name) '(:static sCBasev) '(:static abc) '(:static sC1v) 'I1v 'IBase2v 'I2v 'IBase1v 'C1v 'CBasev)
(list-has-values? (oo-properties I1) '(:static name) '(:static sIBase1v) '(:static sIBase2v) '(:static sI1v) 'I1v 'IBase1v 'IBase2v)

(ut o1.class C1)
(ut o1.class.name 'C1)
(ut I1.name 'I1)

; Show class <self> & <super> polymorphism works
; => Note this doesn't apply to <interface>s since they don't have <super>
(define-class Base
  (:static sval 0)
  (val 0)
  (:static (sf) self.sval)
  ((f) self.val))

(define-class Middle (:extends Base)
  (:static sval 1)
  (val 1)
  (:static (scheck-super) (super.sf))
  ((check-super) (super.f)))

(define-class Topmost (:extends Middle)
  (:static sval 2)
  (val 2))

(ut (Base.sf) 0)
(ut (Middle.sf) 1)
(ut (Topmost.sf) 2)

(ut (Middle.scheck-super) 0)
(ut (Topmost.scheck-super) 0)

(define obase (Base))
(define omiddle (Middle))
(define otopmost (Topmost))
(ut (obase.f) 0)
(ut (omiddle.f) 1)
(ut (otopmost.f) 2)

(ut (omiddle.check-super) 0)
(ut (otopmost.check-super) 0)

; Show interface static methods can access <self>
(define-interface I
  (:static val 0)
  (:static (f) self.val))

(ut (I.f) 0)

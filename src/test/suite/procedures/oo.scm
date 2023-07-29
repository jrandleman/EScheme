;; Author: Jordan Randleman -- oo.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (meta-object? 0) #f)
(ut (meta-object? (class)) #t)
(ut (meta-object? ((class))) #t)
(ut (meta-object? (interface)) #t)

(import (path #path ".." ".." "examples" "procedures" "system") module-test)
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
(ut (oo-is? (C1) C1) #t)
(ut (oo-is? (C1) CBase) #t)
(ut (oo-is? (C1) I1) #t)
(ut (oo-is? (C1) I2) #t)
(ut (oo-is? (C1) IBase1) #t)
(ut (oo-is? (C1) IBase2) #t)
(ut (oo-is? (C2) C2) #t)
(ut (oo-is? (C2) CBase) #f)
(ut (oo-is? (C2) I1) #f)
(ut (oo-is? (C2) I2) #f)
(ut (oo-is? (C2) IBase1) #f)
(ut (oo-is? (C2) IBase2) #f)

(ut (oo-has? (C1) 'C1v) #t)
(ut (oo-has? (C1) 'C2v) #f)
(ut (oo-has? (C1) 'CBasev) #t)
(ut (oo-has? (C1) 'I1v) #t)
(ut (oo-has? (C1) 'I2v) #t)
(ut (oo-has? (C1) 'IBase1v) #t)
(ut (oo-has? (C1) 'IBase2v) #t)
(ut (oo-has? (C2) 'C2v) #t)
(ut (oo-has? (C2) 'C1v) #f)
(ut (oo-has? (C2) 'CBasev) #f)
(ut (oo-has? (C2) 'I1v) #f)
(ut (oo-has? (C2) 'I2v) #f)
(ut (oo-has? (C2) 'IBase1v) #f)
(ut (oo-has? (C2) 'IBase2v) #f)

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

(ut (oo-get C1 'sC1v) 'sC1v)
(ut (oo-get C1 'sCBasev) 'sCBasev)
(ut (oo-get C2 'sC2v) 'sC2v)
(ut (oo-get I1 'sI1v) 'sI1v)
(ut (oo-get I2 'sI2v) 'sI2v)

(ut (begin (oo-set! C1 'sC1v 1) C1.sC1v) 1)
(ut (begin (oo-set! C1 'sCBasev 2) C1.sCBasev) 2)
(ut (begin (oo-set! C2 'sC2v 3) C2.sC2v) 3)
(ut (begin (oo-set! I1 'sI1v 4) I1.sI1v) 4)
(ut (begin (oo-set! I2 'sI2v 5) I2.sI2v) 5)

(ut (begin (oo-set! o1 'C1v 1) o1.C1v) 1)
(ut (begin (oo-set! o1 'CBasev 2) o1.CBasev) 2)
(ut (begin (oo-set! o1 'I1v 4) o1.I1v) 4)
(ut (begin (oo-set! o1 'I2v 5) o1.I2v) 5)

(ut (begin (oo-define o1 'abc 77) o1.abc) 77)
(ut (begin (oo-define C1 'abc 88) C1.abc) 88)
(ut (begin (oo-define I2 'abc 99) I2.abc) 99)

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
(list-has-values? (oo-properties IBase1) '(:static name) '(:static sIBase1v) 'IBase1v)

(ut o1.class C1)
(ut o1.class.name 'C1)
(ut I1.name 'I1)

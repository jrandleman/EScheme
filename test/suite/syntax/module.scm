;; Author: Jordan Randleman -- module.scm
;; => Tests for EScheme's primitive module syntax.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define module-dir (path (path-parent #path 2) "examples" "specials"))
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

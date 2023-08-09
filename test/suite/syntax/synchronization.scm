;; Author: Jordan Randleman -- synchronization.scm
;; => Tests for EScheme's primitive synchronization syntax.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
; <dosync> (etc.) correctness across threads is checked by "./procedures/concurrent.scm"
(define module-dir (path (path-parent #path 2) "examples" "specials"))
(import module-dir module1)
(ut (syntax? dosync) #t)
(ut (syntax? dosync-module) #t)
(ut (syntax? dosync-with) #t)
(ut (eq? module1.*dosync-module-lock* *dosync-module-lock*) #f) ; module-specific (non-parameter)
(ut (eq? module1.*dosync-lock* *dosync-lock*) #t) ; module-global (parameter)

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
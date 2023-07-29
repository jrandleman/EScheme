;; Author: Jordan Randleman -- concurrent.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES
(define hm {})
(define counter 0)
(define counter2 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS & SYNTAX
(define (++counter-till-interrupted)
  (define test #t)
  (while (test) 
    (if (interrupted?!)
        (set! test #f)
        (set! counter (+ counter 1)))))

(define (++counter2-forever)
  (while (#t) 
    (set! counter2 (+ counter2 1))))

(define-syntax sleep-while
  (lambda (c)
    `(while (,c) (sleep 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

;; THREADS
(define t1 (thread (lambda () (hashmap-set! hm (gensym) 42))))
(define t2 (thread "t2" (lambda () (hashmap-set! hm (gensym) 314))))

(ut (null? (thread-start! t1 t2)) #t)
(thread-join! t1)
(thread-join! t2)
(ut (length hm) 2)

(ut (thread? t1) #t)
(ut (thread? t2) #t)
(ut (thread? 0) #f)
(ut (thread? (lambda (x) x)) #f)
(ut (thread-name t2) "t2")
(ut (= (thread-id t1) (thread-id t2)) #f)
(ut (callable? (thread-runnable t1)) #t)
(ut (callable? (thread-runnable t2)) #t)
(ut (begin ((thread-runnable t1)) (length hm)) 3)

(ut (thread-status t1) 'finished)
(define t3 (thread ++counter-till-interrupted)) ; runs until interrupted
(ut (thread-status t3) 'ready)

(ut (null? (thread-start! t3)) #t)
(thread-join! t3 100) ; max out wait time to 100ms
(ut (thread-interrupted? t3) #f)
(thread-interrupt! t3)
(ut (positive? counter) #t)

(ut (thread-yield) #void) ; no real test for this we can check, besides that the funciton is callable

(define t4 (thread ++counter2-forever)) ; runs infinitely
(ut (thread-set-daemon! t4 #t) #t)
(ut (thread-daemon? t4) #t)
(ut (thread-daemon? t3) #f)
(ut (thread-set-daemon! t4 #f) #t)
(ut (thread-daemon? t4) #f)
(thread-set-daemon! t4 #t)
(thread-start! t4) ; starting this infinite-runnable deamon thread is fine, since deamons don't prevent the process from exiting
(thread-interrupt! t4)
(ut (thread-interrupted? t4) #t)

(ut (integer? (thread-priority t4)) #t)
(if (= (thread-priority t4) *max-priority*)
    (ut (thread-set-priority! t4 (- *max-priority* 1)) #t)
    (ut (thread-set-priority! t4 (+ (thread-priority t4) 1)) #t))

(define start (epoch-time))
(sleep 1000)
(define end (epoch-time))
(ut (>= (- end start) 1000) #t)

(ut (thread? (current-thread)) #t)

(set! hm {})
(define (f1)
  (let loop1 ((counter 0))
    (hashmap-set! hm (gensym) counter)
    (if (< counter 9)
        (loop1 (+ counter 1)))))
(define (f2)
  (let loop2 ((counter 0))
    (hashmap-set! hm (gensym) (+ 10 counter))
    (if (< counter 9)
        (loop2 (+ counter 1)))))
(define (f3)
  (let loop3 ((counter 0))
    (hashmap-set! hm (gensym) (+ 100 counter))
    (if (< counter 9)
        (loop3 (+ counter 1)))))
(parallel f1 f2 f3)
(sleep-while (< (length hm) 30)) ; wait for all 3 threads to finish

(ut (list? (thread-dynamic-environment (current-thread))) #t)


;; MUTEXES
(ut (define m1 (mutex)) #void)
(ut (define m2 (mutex "m2")) #void)

(ut (mutex? m1) #t)
(ut (mutex? m2) #t)
(ut (mutex? (current-thread)) #f)
(ut (mutex? 0) #f)

(ut (mutex-name m2) "m2")

(ut (mutex-set-specific! m1 1/4) #void)
(ut (mutex-specific m1) 1/4)

(ut (mutex-lock! m1) #t)
(ut (mutex-lock! m1) #t) ; re-entrant (recursive) locks!
(ut (mutex-lock! m2 10000) #t)

(ut (mutex-locked? m1) #t)
(ut (mutex-locked? m2) #t)

(ut (begin (mutex-unlock! m1) (mutex-unlock! m1) (mutex-locked? m1)) #f) ; unlock twice due to locking twice before
(ut (begin (mutex-unlock! m2) (mutex-locked? m2)) #f)

; We now try to show that the mutex is actually locking code across threads,
;   rather than just claiming to be locked/unlocked.
; Note that this can be inherently difficult, since the JVM's thread-scheduler 
;   is non-deterministic. Hence we design an algorithm that has a very low 
;   change for #f positives, and no chance for #f negatives. Then, we use
;   "amplification" (a quick thank you to my graduate algorithms course!)
;   in order to even further reduce the liklihood of error.
(define *amplification-count* 10)

(define *buffer* "")

(define *f-end* #f)
(define (f)
  (let loop ((n 0))
    (dosync-with m1
      (set! *buffer* (conj #\a *buffer*))
      (set! *buffer* (conj #\b *buffer*))
      (set! *buffer* (conj #\c *buffer*))
      (set! *buffer* (conj #\d *buffer*))
      (set! *buffer* (conj #\e *buffer*))
      (set! *buffer* (conj #\f *buffer*))
      (set! *buffer* (conj #\g *buffer*))
      (set! *buffer* (conj #\h *buffer*))
      (set! *buffer* (conj #\i *buffer*))
      (set! *buffer* (conj #\j *buffer*))
      (set! *buffer* (conj #\k *buffer*))
      (set! *buffer* (conj #\l *buffer*))
      (set! *buffer* (conj #\m *buffer*))
      (set! *buffer* (conj #\n *buffer*))
      (set! *buffer* (conj #\o *buffer*))
      (set! *buffer* (conj #\p *buffer*))
      (set! *buffer* (conj #\q *buffer*))
      (set! *buffer* (conj #\r *buffer*))
      (set! *buffer* (conj #\s *buffer*))
      (set! *buffer* (conj #\t *buffer*))
      (set! *buffer* (conj #\u *buffer*))
      (set! *buffer* (conj #\v *buffer*))
      (set! *buffer* (conj #\w *buffer*))
      (set! *buffer* (conj #\x *buffer*))
      (set! *buffer* (conj #\y *buffer*))
      (set! *buffer* (conj #\z *buffer*))
      (set! *buffer* (conj #\newline *buffer*)))
    (if (< n 1000)
        (loop (+ n 1))
        (set! *f-end* #t))))

(define *g-end* #f)
(define (g)
  (let loop ((n 0))
    (dosync-with m1
      (set! *buffer* (conj #\A *buffer*))
      (set! *buffer* (conj #\B *buffer*))
      (set! *buffer* (conj #\C *buffer*))
      (set! *buffer* (conj #\D *buffer*))
      (set! *buffer* (conj #\E *buffer*))
      (set! *buffer* (conj #\F *buffer*))
      (set! *buffer* (conj #\G *buffer*))
      (set! *buffer* (conj #\H *buffer*))
      (set! *buffer* (conj #\I *buffer*))
      (set! *buffer* (conj #\J *buffer*))
      (set! *buffer* (conj #\K *buffer*))
      (set! *buffer* (conj #\L *buffer*))
      (set! *buffer* (conj #\M *buffer*))
      (set! *buffer* (conj #\N *buffer*))
      (set! *buffer* (conj #\O *buffer*))
      (set! *buffer* (conj #\P *buffer*))
      (set! *buffer* (conj #\Q *buffer*))
      (set! *buffer* (conj #\R *buffer*))
      (set! *buffer* (conj #\S *buffer*))
      (set! *buffer* (conj #\T *buffer*))
      (set! *buffer* (conj #\U *buffer*))
      (set! *buffer* (conj #\V *buffer*))
      (set! *buffer* (conj #\W *buffer*))
      (set! *buffer* (conj #\X *buffer*))
      (set! *buffer* (conj #\Y *buffer*))
      (set! *buffer* (conj #\Z *buffer*))
      (set! *buffer* (conj #\newline *buffer*)))
    (if (< n 1000)
        (loop (+ n 1))
        (set! *g-end* #t))))

; Note that this could _theoretically_ cause a #f positive,
; though even then _technically_ the mutually exclusive printing
; behavior will have been preserved (randomly!) by the JVM's thread
; scheduler.
;   => We reduce the liklihood of this #f positive via amplification!
(define (mutex-preserved?)
  (define mutex-strings (string-split *buffer* "\n"))
  (eq? 
    (count (lambda (str)
              (or (eq? "abcdefghijklmnopqrstuvwxyz" str) 
                  (eq? "ABCDEFGHIJKLMNOPQRSTUVWXYZ" str)))
           mutex-strings)
    (length mutex-strings)))

(do ((i 0 (+ i 1)))
    ((= i *amplification-count*))
    (ut (begin 
          'testing-mutex-lock! 
          (set! *buffer* "")
          (set! *f-end* #f)
          (set! *g-end* #f)
          (parallel f g)
          (sleep-while (not (and *f-end* *g-end*))) ; wait for both threads to finish
          (mutex-preserved?)) 
        #t))

(mutex-lock! m1)
(define t5 (thread (lambda () (mutex-lock! m1) (mutex-unlock! m1))))
(thread-start! t5)
(sleep-while (not (mutex-queued? m1)))
(ut (mutex-queue-length m1) 1)
(mutex-unlock! m1)
(sleep-while (mutex-locked? m1))
(ut (mutex-queue-length m1) 0)

(ut (mutex-hold-count m1) 0)
(ut (mutex-held? m1) #f)
(mutex-lock! m1)
(ut (mutex-hold-count m1) 1)
(ut (mutex-held? m1) #t)
(mutex-lock! m1)
(ut (mutex-hold-count m1) 2)
(ut (mutex-held? m1) #t)
(mutex-unlock! m1)
(ut (mutex-hold-count m1) 1)
(ut (mutex-held? m1) #t)
(mutex-unlock! m1)
(ut (mutex-hold-count m1) 0)
(ut (mutex-held? m1) #f)


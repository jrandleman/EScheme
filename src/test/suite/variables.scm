;; Author: Jordan Randleman -- variables.scm
;; => Tests for EScheme's global variables.
;;    Invoked by ../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (string? #path) #t)
(ut (null? #nil) #t)
(ut (eof? #eof) #t)
(ut (void? #void) #t)

(ut (list? *argv*) #t)

(ut (string? *file-separator*) #t)
(ut (string? *path-separator*) #t)

(ut (string? *os-name*) #t)
(ut (string? *os-version*) #t)
(ut (string? *os-architecture*) #t)

(ut (string? *escm-path*) #t)
(ut (string? *escm-execution-command*) #t)

(ut (and (parameter? *dosync-lock*) (mutex? *dosync-lock*) (not (mutex-locked? *dosync-lock*))) #t)
(ut (and (not (parameter? *dosync-module-lock*)) (mutex? *dosync-module-lock*) (not (mutex-locked? *dosync-module-lock*))) #t)

(ut *generator-complete* *generator-complete*) ; just verify it exists: indeterminate value

(ut (number? *min-radix*) #t)
(ut (number? *max-radix*) #t)
(ut (< *min-radix* *max-radix*) #t)

(ut (number? *min-priority*) #t)
(ut (number? *max-priority*) #t)
(ut (< *min-priority* *max-priority*) #t)

(ut (boolean? *import*) #t)

(ut (and (not (parameter? *load-once-files*)) (hashmap? *load-once-files*)) #t)

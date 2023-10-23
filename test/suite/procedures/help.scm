;; Author: Jordan Randleman -- help.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

; NO TESTS FOR INTERACTIVE HELP MENU:
;   => TOO DYNAMIC
;   => ALWAYS PRINTS TO & READS FROM SYSTEM'S <stdout>/<stdin>, REGARDLESS
;      OF `(current-output-port)` OR `(current-input-port)` VALUES

(ut (help +) #void) ; test can help is callable with an object

(ut (define-help "Procedures:Numbers:factorial" "Returns N*(N-1)*...*2*1 for integer N") #void)
(ut define-help defhelp) ; check alias

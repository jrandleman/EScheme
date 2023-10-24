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
(ut (string? (help "Procedures:Numbers")) #t) ; test can get help folder contents as a string
(ut (string? (help "Procedures:Numbers:+")) #t) ; test can get help topic contents as a string

(ut (define-help "Procedures:Numbers:factorial" "Returns N*(N-1)*...*2*1 for integer N") #void)
(ut (string? (help "Procedures:Numbers:factorial")) #t) ; verify actually defined the new section
(ut define-help defhelp) ; check alias

(ut (define help-root (help-directory)) #void) ; check can call <help-directory>
(ut (and (list? help-root) (car help-root)) :~) ; check at least have the home directory
;; Author: Jordan Randleman -- main.scm
;; => Defines EScheme's Stdlib Testing Suite Dispatcher

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST SUIT PROCESSING HELPERS
;;   Execute every file as a testing script in the <suite> directory.
(define total-files 0)
(define total-errors 0)
(define verbose-mode #t)

(define (verbose-displayf . args)
  (if verbose-mode
      (apply displayf args)))

(define (execute-test-file file)
  (define test-file-name (slice file (+ 1 (length *escm-path*))))
  (set! total-files (+ total-files 1))
  (verbose-displayf "    %n) Executing Test File: %wa ..." total-files test-file-name)
  (define time-result (time system (append *escm-execution-command* file)))
  (define time-length (car time-result))
  (define test-result (cdr time-result))
  (define error-code (caddr test-result))
  (verbose-displayf " Time: %nms; Exit Code: %n\n" time-length error-code)
  (if (not (= 0 error-code))
      (set! total-errors (+ total-errors 1)))
  (define err-msg (cadr test-result))
  (if (not (empty? err-msg))
      (verbose-displayf "\n>>> Error Processing File %wa:\n%a\n\n" test-file-name err-msg)))

(define (test-all-files dir)
  (for-each 
    (lambda (file)
      (if (directory? file)
          (test-all-files file)
          (execute-test-file file)))
    (directory-entries* dir))) ; don't parse dot-files!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN EXECUTION
(define (tests-main-execution)
  (verbose-displayf ">>> Processing Test Files:\n")
  (define start (epoch-time))
  (test-all-files (path #path "suite"))
  (define end (epoch-time))
  (verbose-displayf ">>> Total Files: %n; Total Errors: %n; Success Rate: %.2n%%; Total Time: %.2ns\n" 
    total-files 
    total-errors 
    (* 100.0 (/ (- total-files total-errors) total-files))
    (/ (- end start) 1000.0))
  (exit total-errors))

(tests-main-execution)

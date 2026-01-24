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
  (set! total-files (+ total-files 1))
  (define test-file-name (slice file (+ (length *file-separator*) (length *escm-path*))))
  (verbose-displayf "    %n) Executing Test File: %wa ..." total-files test-file-name)
  (define time-length test-result (time system (append *escm-execution-command* "\"" file "\"")))
  (define output errput errcode test-result)
  (define result-emoji (if (zero? errcode) "\u2705" "\ud83d\udea8"))
  (verbose-displayf " Time: %nms; Exit Code: %n %s\n" time-length errcode result-emoji)
  (if (not (= 0 errcode))
      (set! total-errors (+ total-errors 1)))
  (if (not (empty? errput))
      (verbose-displayf "\n>>> Error Processing File %wa:\n%a\n\n" test-file-name errput)))

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
  (verbose-displayf ">>> Processing Test Files (please be patient \ud83d\ude05):\n")
  (define start (epoch-time))
  (test-all-files (path #path "suite"))
  (define end (epoch-time))
  (define result-emoji (if (zero? total-errors) "\ud83d\udc4d" "\ud83d\udc4e"))
  (verbose-displayf ">>> Total Files: %n; Total Errors: %n; Success Rate: %.2n%%; Total Time: %.2ns %s\n" 
    total-files 
    total-errors 
    (* 100.0 (/ (- total-files total-errors) total-files))
    (/ (- end start) 1000.0)
    result-emoji)
  (exit total-errors))

(tests-main-execution)

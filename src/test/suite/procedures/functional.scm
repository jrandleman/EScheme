;; Author: Jordan Randleman -- functional.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(def v [])
(ut (id 314) 314)
(ut eq? (id v) v)

(ut (procedure? +) #t)
(ut (procedure? if) #t)
(ut (procedure? #f) #f)

(ut (callable? +) #t)
(ut (callable? if) #t)
(ut (callable? []) #t)
(ut (callable? {}) #t)
(ut (callable? "") #t)
(ut (callable? ()) #f)
(ut (callable? (class)) #t)
(ut (callable? ((class ((->procedure) #f)))) #t)
(ut (callable? #f) #f)

(ut ((compose even? length) []) #t)
(ut ((compose even? length) [3]) #f)
(ut ((compose even? length) [3 1]) #t)
(ut ((compose even? length) [3 1 4]) #f)

(ut ((bind + 1) 2) 3)
(ut ((bind + 1 2) 3) 6)
(ut ((bind + 1 2 3) 4) 10)

(ut ((bind + 1) 2 1) 4)
(ut ((bind + 1 2) 3 1) 7)
(ut ((bind + 1 2 3) 4 1) 11)

(ut ((compose (bind * 3) (bind * 2) length) []) 0)
(ut ((compose (bind * 3) (bind * 2) length) [1]) 6)
(ut ((compose (bind * 3) (bind * 2) length) [1 2 3]) 18)
(ut ((compose even? (bind * 3) (bind * 2) length) []) #t)
(ut ((compose even? (bind * 3) (bind * 2) length) [1]) #t)
(ut ((compose even? (bind * 3) (bind * 2) length) [1 2 3]) #t)
(ut ((compose - +) 1 2 3) -6)

(ut ((* even? length) []) #t)
(ut ((* even? length) [3]) #f)
(ut ((* even? length) [3 1]) #t)
(ut ((* even? length) [3 1 4]) #f)

(ut ((+ + 1) 2) 3)
(ut ((+ + 1 2) 3) 6)
(ut ((+ + 1 2 3) 4) 10)

(ut ((+ + 1) 2 1) 4)
(ut ((+ + 1 2) 3 1) 7)
(ut ((+ + 1 2 3) 4 1) 11)

(ut ((* (+ * 3) (+ * 2) length) []) 0)
(ut ((* (+ * 3) (+ * 2) length) [1]) 6)
(ut ((* (+ * 3) (+ * 2) length) [1 2 3]) 18)
(ut ((* even? (+ * 3) (+ * 2) length) []) #t)
(ut ((* even? (+ * 3) (+ * 2) length) [1]) #t)
(ut ((* even? (+ * 3) (+ * 2) length) [1 2 3]) #t)
(ut ((* - +) 1 2 3) -6)

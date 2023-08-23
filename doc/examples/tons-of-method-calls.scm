; tons-of-method-calls.scm
; => Demos performance for half a million method calls (avgs 1000+/ms)
; => This file does not need any cmd-line arguments when being executed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining our Binary Tree Node
(define-class Node
  ((new v)
    (def self.v v)
    (def self.l #f)
    (def self.r #f))
  ((insert! v)
    (cond ((< v self.v)
            (if self.l 
                (self.l.insert! v)
                (set! self.l (Node v))))
          ((> v self.v)
            (if self.r 
                (self.r.insert! v)
                (set! self.r (Node v))))))
  ((inorder f)
    (if self.l (self.l.inorder f))
    (f self.v)
    (if self.r (self.r.inorder f)))
  ((preorder f)
    (f self.v)
    (if self.l (self.l.preorder f))
    (if self.r (self.r.preorder f)))
  ((postorder f)
    (if self.l (self.l.postorder f))
    (if self.r (self.r.postorder f))
    (f self.v)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining our Binary Tree Handle
(define-class Tree
  ((new . vals)
    (def self.root #f)
    (for-each self.insert! vals))
  ((insert! v)
    (if self.root
        (self.root.insert! v)
        (set! self.root (Node v))))
  ((inorder f)
    (if self.root (self.root.inorder f)))
  ((preorder f)
    (if self.root (self.root.preorder f)))
  ((postorder f)
    (if self.root (self.root.postorder f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measuring Performance of 500500 method calls
(def t (Tree))
(def count 0)

(def start (epoch-time))
(while ((<= count 1000))
  (t.insert! count)
  (set! count (+ count 1)))
(def end (epoch-time))

(def total-ms (- end start))
(printf "> Total time for 500500 method-calls: %nms\n" total-ms)
(printf "> Average number of method-calls per ms: %en\n" (floor (/ 500500 total-ms)))
(printf "> Printing the numbers 0...1000 backwards to verify correctness:\n")
(t.postorder #(printf "%a " %1))
(newline)

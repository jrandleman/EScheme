(defn fact
  ((n) (fact n 1))
  ((n p) (if (< n 2) p (fact (- n 1) (* n p)))))

(display (fact 5))
(newline)
(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (negative? 0) '#f)
(nut equal? (negative? 0.0) '#f)
(nut equal? (negative? Infinity) '#f)
(nut equal? (negative? -Infinity) '#t)
(nut equal? (negative? NaN) '#f)
(nut equal? (negative? 1) '#f)
(nut equal? (negative? 4) '#f)
(nut equal? (negative? -1) '#t)
(nut equal? (negative? -4) '#t)
(nut equal? (negative? 1.0) '#f)
(nut equal? (negative? 4.0) '#f)
(nut equal? (negative? -1.0) '#t)
(nut equal? (negative? -4.0) '#t)
(nut equal? (negative? 1/2) '#f)
(nut equal? (negative? 1/8) '#f)
(nut equal? (negative? -1/2) '#t)
(nut equal? (negative? -1/8) '#t)

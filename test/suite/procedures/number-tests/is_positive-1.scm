(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (positive? 0) '#f)
(nut equal? (positive? 0.0) '#f)
(nut equal? (positive? Infinity) '#t)
(nut equal? (positive? -Infinity) '#f)
(nut equal? (positive? NaN) '#f)
(nut equal? (positive? 1) '#t)
(nut equal? (positive? 4) '#t)
(nut equal? (positive? -1) '#f)
(nut equal? (positive? -4) '#f)
(nut equal? (positive? 1.0) '#t)
(nut equal? (positive? 4.0) '#t)
(nut equal? (positive? -1.0) '#f)
(nut equal? (positive? -4.0) '#f)
(nut equal? (positive? 1/2) '#t)
(nut equal? (positive? 1/8) '#t)
(nut equal? (positive? -1/2) '#f)
(nut equal? (positive? -1/8) '#f)

(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (finite? 0) '#t)
(nut equal? (finite? 0.0) '#t)
(nut equal? (finite? Infinity) '#f)
(nut equal? (finite? -Infinity) '#f)
(nut equal? (finite? NaN) '#f)
(nut equal? (finite? 1) '#t)
(nut equal? (finite? 4) '#t)
(nut equal? (finite? -1) '#t)
(nut equal? (finite? -4) '#t)
(nut equal? (finite? 1.0) '#t)
(nut equal? (finite? 4.0) '#t)
(nut equal? (finite? -1.0) '#t)
(nut equal? (finite? -4.0) '#t)
(nut equal? (finite? 1/2) '#t)
(nut equal? (finite? 1/8) '#t)
(nut equal? (finite? -1/2) '#t)
(nut equal? (finite? -1/8) '#t)

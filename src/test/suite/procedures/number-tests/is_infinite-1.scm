(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (infinite? 0) '#f)
(nut equal? (infinite? 0.0) '#f)
(nut equal? (infinite? Infinity) '#t)
(nut equal? (infinite? -Infinity) '#t)
(nut equal? (infinite? NaN) '#f)
(nut equal? (infinite? 1) '#f)
(nut equal? (infinite? 4) '#f)
(nut equal? (infinite? -1) '#f)
(nut equal? (infinite? -4) '#f)
(nut equal? (infinite? 1.0) '#f)
(nut equal? (infinite? 4.0) '#f)
(nut equal? (infinite? -1.0) '#f)
(nut equal? (infinite? -4.0) '#f)
(nut equal? (infinite? 1/2) '#f)
(nut equal? (infinite? 1/8) '#f)
(nut equal? (infinite? -1/2) '#f)
(nut equal? (infinite? -1/8) '#f)

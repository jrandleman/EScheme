(load "/Users/jordanrandleman/Desktop/EScheme/src/test/suite/procedures/../../lib.scm")
(nut equal? (zero? 0) '#t)
(nut equal? (zero? 0.0) '#t)
(nut equal? (zero? Infinity) '#f)
(nut equal? (zero? -Infinity) '#f)
(nut equal? (zero? NaN) '#f)
(nut equal? (zero? 1) '#f)
(nut equal? (zero? 4) '#f)
(nut equal? (zero? -1) '#f)
(nut equal? (zero? -4) '#f)
(nut equal? (zero? 1.0) '#f)
(nut equal? (zero? 4.0) '#f)
(nut equal? (zero? -1.0) '#f)
(nut equal? (zero? -4.0) '#f)
(nut equal? (zero? 1/2) '#f)
(nut equal? (zero? 1/8) '#f)
(nut equal? (zero? -1/2) '#f)
(nut equal? (zero? -1/8) '#f)
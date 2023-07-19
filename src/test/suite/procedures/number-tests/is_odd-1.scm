(load "/Users/jordanrandleman/Desktop/EScheme/src/test/suite/procedures/../../lib.scm")
(nut equal? (odd? 0) '#f)
(nut equal? (odd? 0.0) '#f)
:nop
:nop
:nop
(nut equal? (odd? 1) '#t)
(nut equal? (odd? 4) '#f)
(nut equal? (odd? -1) '#t)
(nut equal? (odd? -4) '#f)
(nut equal? (odd? 1.0) '#t)
(nut equal? (odd? 4.0) '#f)
(nut equal? (odd? -1.0) '#t)
(nut equal? (odd? -4.0) '#f)
:nop
:nop
:nop
:nop

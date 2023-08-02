(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (even? 0) '#t)
(nut equal? (even? 0.0) '#t)
:nop
:nop
:nop
(nut equal? (even? 1) '#f)
(nut equal? (even? 4) '#t)
(nut equal? (even? -1) '#f)
(nut equal? (even? -4) '#t)
(nut equal? (even? 1.0) '#f)
(nut equal? (even? 4.0) '#t)
(nut equal? (even? -1.0) '#f)
(nut equal? (even? -4.0) '#t)
:nop
:nop
:nop
:nop

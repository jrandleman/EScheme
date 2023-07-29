(load (path #path ".." ".." ".." "lib.scm"))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 0 1) '(0 . 0))
:nop
(nut equal? (divrem 0 4) '(0 . 0))
:nop
(nut equal? (divrem 0 -1) '(0 . 0))
:nop
(nut equal? (divrem 0 -4) '(0 . 0))
:nop
(nut equal? (divrem 0.0 1.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 4.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -1.0) '(-0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -4.0) '(-0.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 0.0 1.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 4.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -1.0) '(-0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -4.0) '(-0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 1.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 4.0) '(0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -1.0) '(-0.0 . 0.0))
:nop
(nut equal? (divrem 0.0 -4.0) '(-0.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 1 4) '(0 . 1))
(nut equal? (divrem 4 1) '(4 . 0))
(nut equal? (divrem 1 -1) '(-1 . 0))
(nut equal? (divrem -1 1) '(-1 . 0))
(nut equal? (divrem 1 -4) '(0 . 1))
(nut equal? (divrem -4 1) '(-4 . 0))
(nut equal? (divrem 1.0 1.0) '(1.0 . 0.0))
(nut equal? (divrem 1.0 1.0) '(1.0 . 0.0))
(nut equal? (divrem 1.0 4.0) '(0.0 . 1.0))
(nut equal? (divrem 4.0 1.0) '(4.0 . 0.0))
(nut equal? (divrem 1.0 -1.0) '(-1.0 . 0.0))
(nut equal? (divrem -1.0 1.0) '(-1.0 . 0.0))
(nut equal? (divrem 1.0 -4.0) '(-0.0 . 1.0))
(nut equal? (divrem -4.0 1.0) '(-4.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 4 -1) '(-4 . 0))
(nut equal? (divrem -1 4) '(0 . -1))
(nut equal? (divrem 4 -4) '(-1 . 0))
(nut equal? (divrem -4 4) '(-1 . 0))
(nut equal? (divrem 4.0 1.0) '(4.0 . 0.0))
(nut equal? (divrem 1.0 4.0) '(0.0 . 1.0))
(nut equal? (divrem 4.0 4.0) '(1.0 . 0.0))
(nut equal? (divrem 4.0 4.0) '(1.0 . 0.0))
(nut equal? (divrem 4.0 -1.0) '(-4.0 . 0.0))
(nut equal? (divrem -1.0 4.0) '(-0.0 . -1.0))
(nut equal? (divrem 4.0 -4.0) '(-1.0 . 0.0))
(nut equal? (divrem -4.0 4.0) '(-1.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem -1 -4) '(0 . -1))
(nut equal? (divrem -4 -1) '(4 . 0))
(nut equal? (divrem -1.0 1.0) '(-1.0 . 0.0))
(nut equal? (divrem 1.0 -1.0) '(-1.0 . 0.0))
(nut equal? (divrem -1.0 4.0) '(-0.0 . -1.0))
(nut equal? (divrem 4.0 -1.0) '(-4.0 . 0.0))
(nut equal? (divrem -1.0 -1.0) '(1.0 . 0.0))
(nut equal? (divrem -1.0 -1.0) '(1.0 . 0.0))
(nut equal? (divrem -1.0 -4.0) '(0.0 . -1.0))
(nut equal? (divrem -4.0 -1.0) '(4.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem -4.0 1.0) '(-4.0 . 0.0))
(nut equal? (divrem 1.0 -4.0) '(-0.0 . 1.0))
(nut equal? (divrem -4.0 4.0) '(-1.0 . 0.0))
(nut equal? (divrem 4.0 -4.0) '(-1.0 . 0.0))
(nut equal? (divrem -4.0 -1.0) '(4.0 . 0.0))
(nut equal? (divrem -1.0 -4.0) '(0.0 . -1.0))
(nut equal? (divrem -4.0 -4.0) '(1.0 . 0.0))
(nut equal? (divrem -4.0 -4.0) '(1.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 1.0 4.0) '(0.0 . 1.0))
(nut equal? (divrem 4.0 1.0) '(4.0 . 0.0))
(nut equal? (divrem 1.0 -1.0) '(-1.0 . 0.0))
(nut equal? (divrem -1.0 1.0) '(-1.0 . 0.0))
(nut equal? (divrem 1.0 -4.0) '(-0.0 . 1.0))
(nut equal? (divrem -4.0 1.0) '(-4.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem 4.0 -1.0) '(-4.0 . 0.0))
(nut equal? (divrem -1.0 4.0) '(-0.0 . -1.0))
(nut equal? (divrem 4.0 -4.0) '(-1.0 . 0.0))
(nut equal? (divrem -4.0 4.0) '(-1.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut equal? (divrem -1.0 -4.0) '(0.0 . -1.0))
(nut equal? (divrem -4.0 -1.0) '(4.0 . 0.0))
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop

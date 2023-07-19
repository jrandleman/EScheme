(load "/Users/jordanrandleman/Desktop/EScheme/src/test/suite/procedures/../../lib.scm")
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo 0.0 NaN) 'NaN)
(nut = (modulo NaN 0.0) 'NaN)
(nut = (modulo 0 1) '0)
:nop
(nut = (modulo 0 4) '0)
:nop
(nut = (modulo 0 -1) '0)
:nop
(nut = (modulo 0 -4) '0)
:nop
(nut = (modulo 0.0 1.0) '0.0)
:nop
(nut = (modulo 0.0 4.0) '0.0)
:nop
(nut = (modulo 0.0 -1.0) '0.0)
:nop
(nut = (modulo 0.0 -4.0) '0.0)
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
(nut = (modulo 0.0 NaN) 'NaN)
(nut = (modulo NaN 0.0) 'NaN)
(nut = (modulo 0.0 1.0) '0.0)
:nop
(nut = (modulo 0.0 4.0) '0.0)
:nop
(nut = (modulo 0.0 -1.0) '0.0)
:nop
(nut = (modulo 0.0 -4.0) '0.0)
:nop
(nut = (modulo 0.0 1.0) '0.0)
:nop
(nut = (modulo 0.0 4.0) '0.0)
:nop
(nut = (modulo 0.0 -1.0) '0.0)
:nop
(nut = (modulo 0.0 -4.0) '0.0)
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
(nut = (modulo Infinity NaN) 'NaN)
(nut = (modulo NaN Infinity) 'NaN)
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
(nut = (modulo -Infinity NaN) 'NaN)
(nut = (modulo NaN -Infinity) 'NaN)
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
(nut = (modulo NaN 1.0) 'NaN)
(nut = (modulo 1.0 NaN) 'NaN)
(nut = (modulo NaN 4.0) 'NaN)
(nut = (modulo 4.0 NaN) 'NaN)
(nut = (modulo NaN -1.0) 'NaN)
(nut = (modulo -1.0 NaN) 'NaN)
(nut = (modulo NaN -4.0) 'NaN)
(nut = (modulo -4.0 NaN) 'NaN)
(nut = (modulo NaN 1.0) 'NaN)
(nut = (modulo 1.0 NaN) 'NaN)
(nut = (modulo NaN 4.0) 'NaN)
(nut = (modulo 4.0 NaN) 'NaN)
(nut = (modulo NaN -1.0) 'NaN)
(nut = (modulo -1.0 NaN) 'NaN)
(nut = (modulo NaN -4.0) 'NaN)
(nut = (modulo -4.0 NaN) 'NaN)
(nut = (modulo NaN 0.5) 'NaN)
(nut = (modulo 0.5 NaN) 'NaN)
(nut = (modulo NaN 0.125) 'NaN)
(nut = (modulo 0.125 NaN) 'NaN)
(nut = (modulo NaN -0.5) 'NaN)
(nut = (modulo -0.5 NaN) 'NaN)
(nut = (modulo NaN -0.125) 'NaN)
(nut = (modulo -0.125 NaN) 'NaN)
(nut = (modulo 1 4) '1)
(nut = (modulo 4 1) '0)
(nut = (modulo 1 -1) '0)
(nut = (modulo -1 1) '0)
(nut = (modulo 1 -4) '-3)
(nut = (modulo -4 1) '0)
(nut = (modulo 1.0 1.0) '0.0)
(nut = (modulo 1.0 1.0) '0.0)
(nut = (modulo 1.0 4.0) '1.0)
(nut = (modulo 4.0 1.0) '0.0)
(nut = (modulo 1.0 -1.0) '0.0)
(nut = (modulo -1.0 1.0) '0.0)
(nut = (modulo 1.0 -4.0) '-3.0)
(nut = (modulo -4.0 1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo 4 -1) '0)
(nut = (modulo -1 4) '3)
(nut = (modulo 4 -4) '0)
(nut = (modulo -4 4) '0)
(nut = (modulo 4.0 1.0) '0.0)
(nut = (modulo 1.0 4.0) '1.0)
(nut = (modulo 4.0 4.0) '0.0)
(nut = (modulo 4.0 4.0) '0.0)
(nut = (modulo 4.0 -1.0) '0.0)
(nut = (modulo -1.0 4.0) '3.0)
(nut = (modulo 4.0 -4.0) '0.0)
(nut = (modulo -4.0 4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo -1 -4) '-1)
(nut = (modulo -4 -1) '0)
(nut = (modulo -1.0 1.0) '0.0)
(nut = (modulo 1.0 -1.0) '0.0)
(nut = (modulo -1.0 4.0) '3.0)
(nut = (modulo 4.0 -1.0) '0.0)
(nut = (modulo -1.0 -1.0) '0.0)
(nut = (modulo -1.0 -1.0) '0.0)
(nut = (modulo -1.0 -4.0) '-1.0)
(nut = (modulo -4.0 -1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo -4.0 1.0) '0.0)
(nut = (modulo 1.0 -4.0) '-3.0)
(nut = (modulo -4.0 4.0) '0.0)
(nut = (modulo 4.0 -4.0) '0.0)
(nut = (modulo -4.0 -1.0) '0.0)
(nut = (modulo -1.0 -4.0) '-1.0)
(nut = (modulo -4.0 -4.0) '0.0)
(nut = (modulo -4.0 -4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo 1.0 4.0) '1.0)
(nut = (modulo 4.0 1.0) '0.0)
(nut = (modulo 1.0 -1.0) '0.0)
(nut = (modulo -1.0 1.0) '0.0)
(nut = (modulo 1.0 -4.0) '-3.0)
(nut = (modulo -4.0 1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo 4.0 -1.0) '0.0)
(nut = (modulo -1.0 4.0) '3.0)
(nut = (modulo 4.0 -4.0) '0.0)
(nut = (modulo -4.0 4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (modulo -1.0 -4.0) '-1.0)
(nut = (modulo -4.0 -1.0) '0.0)
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

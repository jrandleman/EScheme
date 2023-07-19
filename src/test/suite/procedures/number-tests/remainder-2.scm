(load "/Users/jordanrandleman/Desktop/EScheme/src/test/suite/procedures/../../lib.scm")
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder 0.0 NaN) 'NaN)
(nut = (remainder NaN 0.0) 'NaN)
(nut = (remainder 0 1) '0)
:nop
(nut = (remainder 0 4) '0)
:nop
(nut = (remainder 0 -1) '0)
:nop
(nut = (remainder 0 -4) '0)
:nop
(nut = (remainder 0.0 1.0) '0.0)
:nop
(nut = (remainder 0.0 4.0) '0.0)
:nop
(nut = (remainder 0.0 -1.0) '0.0)
:nop
(nut = (remainder 0.0 -4.0) '0.0)
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
(nut = (remainder 0.0 NaN) 'NaN)
(nut = (remainder NaN 0.0) 'NaN)
(nut = (remainder 0.0 1.0) '0.0)
:nop
(nut = (remainder 0.0 4.0) '0.0)
:nop
(nut = (remainder 0.0 -1.0) '0.0)
:nop
(nut = (remainder 0.0 -4.0) '0.0)
:nop
(nut = (remainder 0.0 1.0) '0.0)
:nop
(nut = (remainder 0.0 4.0) '0.0)
:nop
(nut = (remainder 0.0 -1.0) '0.0)
:nop
(nut = (remainder 0.0 -4.0) '0.0)
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
(nut = (remainder Infinity NaN) 'NaN)
(nut = (remainder NaN Infinity) 'NaN)
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
(nut = (remainder -Infinity NaN) 'NaN)
(nut = (remainder NaN -Infinity) 'NaN)
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
(nut = (remainder NaN 1.0) 'NaN)
(nut = (remainder 1.0 NaN) 'NaN)
(nut = (remainder NaN 4.0) 'NaN)
(nut = (remainder 4.0 NaN) 'NaN)
(nut = (remainder NaN -1.0) 'NaN)
(nut = (remainder -1.0 NaN) 'NaN)
(nut = (remainder NaN -4.0) 'NaN)
(nut = (remainder -4.0 NaN) 'NaN)
(nut = (remainder NaN 1.0) 'NaN)
(nut = (remainder 1.0 NaN) 'NaN)
(nut = (remainder NaN 4.0) 'NaN)
(nut = (remainder 4.0 NaN) 'NaN)
(nut = (remainder NaN -1.0) 'NaN)
(nut = (remainder -1.0 NaN) 'NaN)
(nut = (remainder NaN -4.0) 'NaN)
(nut = (remainder -4.0 NaN) 'NaN)
(nut = (remainder NaN 0.5) 'NaN)
(nut = (remainder 0.5 NaN) 'NaN)
(nut = (remainder NaN 0.125) 'NaN)
(nut = (remainder 0.125 NaN) 'NaN)
(nut = (remainder NaN -0.5) 'NaN)
(nut = (remainder -0.5 NaN) 'NaN)
(nut = (remainder NaN -0.125) 'NaN)
(nut = (remainder -0.125 NaN) 'NaN)
(nut = (remainder 1 4) '1)
(nut = (remainder 4 1) '0)
(nut = (remainder 1 -1) '0)
(nut = (remainder -1 1) '0)
(nut = (remainder 1 -4) '1)
(nut = (remainder -4 1) '0)
(nut = (remainder 1.0 1.0) '0.0)
(nut = (remainder 1.0 1.0) '0.0)
(nut = (remainder 1.0 4.0) '1.0)
(nut = (remainder 4.0 1.0) '0.0)
(nut = (remainder 1.0 -1.0) '0.0)
(nut = (remainder -1.0 1.0) '0.0)
(nut = (remainder 1.0 -4.0) '1.0)
(nut = (remainder -4.0 1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder 4 -1) '0)
(nut = (remainder -1 4) '-1)
(nut = (remainder 4 -4) '0)
(nut = (remainder -4 4) '0)
(nut = (remainder 4.0 1.0) '0.0)
(nut = (remainder 1.0 4.0) '1.0)
(nut = (remainder 4.0 4.0) '0.0)
(nut = (remainder 4.0 4.0) '0.0)
(nut = (remainder 4.0 -1.0) '0.0)
(nut = (remainder -1.0 4.0) '-1.0)
(nut = (remainder 4.0 -4.0) '0.0)
(nut = (remainder -4.0 4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder -1 -4) '-1)
(nut = (remainder -4 -1) '0)
(nut = (remainder -1.0 1.0) '0.0)
(nut = (remainder 1.0 -1.0) '0.0)
(nut = (remainder -1.0 4.0) '-1.0)
(nut = (remainder 4.0 -1.0) '0.0)
(nut = (remainder -1.0 -1.0) '0.0)
(nut = (remainder -1.0 -1.0) '0.0)
(nut = (remainder -1.0 -4.0) '-1.0)
(nut = (remainder -4.0 -1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder -4.0 1.0) '0.0)
(nut = (remainder 1.0 -4.0) '1.0)
(nut = (remainder -4.0 4.0) '0.0)
(nut = (remainder 4.0 -4.0) '0.0)
(nut = (remainder -4.0 -1.0) '0.0)
(nut = (remainder -1.0 -4.0) '-1.0)
(nut = (remainder -4.0 -4.0) '0.0)
(nut = (remainder -4.0 -4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder 1.0 4.0) '1.0)
(nut = (remainder 4.0 1.0) '0.0)
(nut = (remainder 1.0 -1.0) '0.0)
(nut = (remainder -1.0 1.0) '0.0)
(nut = (remainder 1.0 -4.0) '1.0)
(nut = (remainder -4.0 1.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder 4.0 -1.0) '0.0)
(nut = (remainder -1.0 4.0) '-1.0)
(nut = (remainder 4.0 -4.0) '0.0)
(nut = (remainder -4.0 4.0) '0.0)
:nop
:nop
:nop
:nop
:nop
:nop
:nop
:nop
(nut = (remainder -1.0 -4.0) '-1.0)
(nut = (remainder -4.0 -1.0) '0.0)
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

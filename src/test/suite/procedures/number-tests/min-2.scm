(load (path (path-parent #path 3) "lib.scm"))
(nut = (min 0.0 0.0) '0.0)
(nut = (min 0.0 0.0) '0.0)
(nut = (min 0.0 Infinity) '0.0)
(nut = (min Infinity 0.0) '0.0)
(nut = (min 0.0 -Infinity) '-Infinity)
(nut = (min -Infinity 0.0) '-Infinity)
(nut = (min 0.0 NaN) 'NaN)
(nut = (min NaN 0.0) 'NaN)
(nut = (min 0 1) '0)
(nut = (min 1 0) '0)
(nut = (min 0 4) '0)
(nut = (min 4 0) '0)
(nut = (min 0 -1) '-1)
(nut = (min -1 0) '-1)
(nut = (min 0 -4) '-4)
(nut = (min -4 0) '-4)
(nut = (min 0.0 1.0) '0.0)
(nut = (min 1.0 0.0) '0.0)
(nut = (min 0.0 4.0) '0.0)
(nut = (min 4.0 0.0) '0.0)
(nut = (min 0.0 -1.0) '-1.0)
(nut = (min -1.0 0.0) '-1.0)
(nut = (min 0.0 -4.0) '-4.0)
(nut = (min -4.0 0.0) '-4.0)
(nut = (min 0 1/2) '0)
(nut = (min 1/2 0) '0)
(nut = (min 0 1/8) '0)
(nut = (min 1/8 0) '0)
(nut = (min 0 -1/2) '-1/2)
(nut = (min -1/2 0) '-1/2)
(nut = (min 0 -1/8) '-1/8)
(nut = (min -1/8 0) '-1/8)
(nut = (min 0.0 Infinity) '0.0)
(nut = (min Infinity 0.0) '0.0)
(nut = (min 0.0 -Infinity) '-Infinity)
(nut = (min -Infinity 0.0) '-Infinity)
(nut = (min 0.0 NaN) 'NaN)
(nut = (min NaN 0.0) 'NaN)
(nut = (min 0.0 1.0) '0.0)
(nut = (min 1.0 0.0) '0.0)
(nut = (min 0.0 4.0) '0.0)
(nut = (min 4.0 0.0) '0.0)
(nut = (min 0.0 -1.0) '-1.0)
(nut = (min -1.0 0.0) '-1.0)
(nut = (min 0.0 -4.0) '-4.0)
(nut = (min -4.0 0.0) '-4.0)
(nut = (min 0.0 1.0) '0.0)
(nut = (min 1.0 0.0) '0.0)
(nut = (min 0.0 4.0) '0.0)
(nut = (min 4.0 0.0) '0.0)
(nut = (min 0.0 -1.0) '-1.0)
(nut = (min -1.0 0.0) '-1.0)
(nut = (min 0.0 -4.0) '-4.0)
(nut = (min -4.0 0.0) '-4.0)
(nut = (min 0.0 0.5) '0.0)
(nut = (min 0.5 0.0) '0.0)
(nut = (min 0.0 0.125) '0.0)
(nut = (min 0.125 0.0) '0.0)
(nut = (min 0.0 -0.5) '-0.5)
(nut = (min -0.5 0.0) '-0.5)
(nut = (min 0.0 -0.125) '-0.125)
(nut = (min -0.125 0.0) '-0.125)
(nut = (min Infinity -Infinity) '-Infinity)
(nut = (min -Infinity Infinity) '-Infinity)
(nut = (min Infinity NaN) 'NaN)
(nut = (min NaN Infinity) 'NaN)
(nut = (min Infinity 1.0) '1.0)
(nut = (min 1.0 Infinity) '1.0)
(nut = (min Infinity 4.0) '4.0)
(nut = (min 4.0 Infinity) '4.0)
(nut = (min Infinity -1.0) '-1.0)
(nut = (min -1.0 Infinity) '-1.0)
(nut = (min Infinity -4.0) '-4.0)
(nut = (min -4.0 Infinity) '-4.0)
(nut = (min Infinity 1.0) '1.0)
(nut = (min 1.0 Infinity) '1.0)
(nut = (min Infinity 4.0) '4.0)
(nut = (min 4.0 Infinity) '4.0)
(nut = (min Infinity -1.0) '-1.0)
(nut = (min -1.0 Infinity) '-1.0)
(nut = (min Infinity -4.0) '-4.0)
(nut = (min -4.0 Infinity) '-4.0)
(nut = (min Infinity 0.5) '0.5)
(nut = (min 0.5 Infinity) '0.5)
(nut = (min Infinity 0.125) '0.125)
(nut = (min 0.125 Infinity) '0.125)
(nut = (min Infinity -0.5) '-0.5)
(nut = (min -0.5 Infinity) '-0.5)
(nut = (min Infinity -0.125) '-0.125)
(nut = (min -0.125 Infinity) '-0.125)
(nut = (min -Infinity NaN) 'NaN)
(nut = (min NaN -Infinity) 'NaN)
(nut = (min -Infinity 1.0) '-Infinity)
(nut = (min 1.0 -Infinity) '-Infinity)
(nut = (min -Infinity 4.0) '-Infinity)
(nut = (min 4.0 -Infinity) '-Infinity)
(nut = (min -Infinity -1.0) '-Infinity)
(nut = (min -1.0 -Infinity) '-Infinity)
(nut = (min -Infinity -4.0) '-Infinity)
(nut = (min -4.0 -Infinity) '-Infinity)
(nut = (min -Infinity 1.0) '-Infinity)
(nut = (min 1.0 -Infinity) '-Infinity)
(nut = (min -Infinity 4.0) '-Infinity)
(nut = (min 4.0 -Infinity) '-Infinity)
(nut = (min -Infinity -1.0) '-Infinity)
(nut = (min -1.0 -Infinity) '-Infinity)
(nut = (min -Infinity -4.0) '-Infinity)
(nut = (min -4.0 -Infinity) '-Infinity)
(nut = (min -Infinity 0.5) '-Infinity)
(nut = (min 0.5 -Infinity) '-Infinity)
(nut = (min -Infinity 0.125) '-Infinity)
(nut = (min 0.125 -Infinity) '-Infinity)
(nut = (min -Infinity -0.5) '-Infinity)
(nut = (min -0.5 -Infinity) '-Infinity)
(nut = (min -Infinity -0.125) '-Infinity)
(nut = (min -0.125 -Infinity) '-Infinity)
(nut = (min NaN 1.0) 'NaN)
(nut = (min 1.0 NaN) 'NaN)
(nut = (min NaN 4.0) 'NaN)
(nut = (min 4.0 NaN) 'NaN)
(nut = (min NaN -1.0) 'NaN)
(nut = (min -1.0 NaN) 'NaN)
(nut = (min NaN -4.0) 'NaN)
(nut = (min -4.0 NaN) 'NaN)
(nut = (min NaN 1.0) 'NaN)
(nut = (min 1.0 NaN) 'NaN)
(nut = (min NaN 4.0) 'NaN)
(nut = (min 4.0 NaN) 'NaN)
(nut = (min NaN -1.0) 'NaN)
(nut = (min -1.0 NaN) 'NaN)
(nut = (min NaN -4.0) 'NaN)
(nut = (min -4.0 NaN) 'NaN)
(nut = (min NaN 0.5) 'NaN)
(nut = (min 0.5 NaN) 'NaN)
(nut = (min NaN 0.125) 'NaN)
(nut = (min 0.125 NaN) 'NaN)
(nut = (min NaN -0.5) 'NaN)
(nut = (min -0.5 NaN) 'NaN)
(nut = (min NaN -0.125) 'NaN)
(nut = (min -0.125 NaN) 'NaN)
(nut = (min 1 4) '1)
(nut = (min 4 1) '1)
(nut = (min 1 -1) '-1)
(nut = (min -1 1) '-1)
(nut = (min 1 -4) '-4)
(nut = (min -4 1) '-4)
(nut = (min 1.0 1.0) '1.0)
(nut = (min 1.0 1.0) '1.0)
(nut = (min 1.0 4.0) '1.0)
(nut = (min 4.0 1.0) '1.0)
(nut = (min 1.0 -1.0) '-1.0)
(nut = (min -1.0 1.0) '-1.0)
(nut = (min 1.0 -4.0) '-4.0)
(nut = (min -4.0 1.0) '-4.0)
(nut = (min 1 1/2) '1/2)
(nut = (min 1/2 1) '1/2)
(nut = (min 1 1/8) '1/8)
(nut = (min 1/8 1) '1/8)
(nut = (min 1 -1/2) '-1/2)
(nut = (min -1/2 1) '-1/2)
(nut = (min 1 -1/8) '-1/8)
(nut = (min -1/8 1) '-1/8)
(nut = (min 4 -1) '-1)
(nut = (min -1 4) '-1)
(nut = (min 4 -4) '-4)
(nut = (min -4 4) '-4)
(nut = (min 4.0 1.0) '1.0)
(nut = (min 1.0 4.0) '1.0)
(nut = (min 4.0 4.0) '4.0)
(nut = (min 4.0 4.0) '4.0)
(nut = (min 4.0 -1.0) '-1.0)
(nut = (min -1.0 4.0) '-1.0)
(nut = (min 4.0 -4.0) '-4.0)
(nut = (min -4.0 4.0) '-4.0)
(nut = (min 4 1/2) '1/2)
(nut = (min 1/2 4) '1/2)
(nut = (min 4 1/8) '1/8)
(nut = (min 1/8 4) '1/8)
(nut = (min 4 -1/2) '-1/2)
(nut = (min -1/2 4) '-1/2)
(nut = (min 4 -1/8) '-1/8)
(nut = (min -1/8 4) '-1/8)
(nut = (min -1 -4) '-4)
(nut = (min -4 -1) '-4)
(nut = (min -1.0 1.0) '-1.0)
(nut = (min 1.0 -1.0) '-1.0)
(nut = (min -1.0 4.0) '-1.0)
(nut = (min 4.0 -1.0) '-1.0)
(nut = (min -1.0 -1.0) '-1.0)
(nut = (min -1.0 -1.0) '-1.0)
(nut = (min -1.0 -4.0) '-4.0)
(nut = (min -4.0 -1.0) '-4.0)
(nut = (min -1 1/2) '-1)
(nut = (min 1/2 -1) '-1)
(nut = (min -1 1/8) '-1)
(nut = (min 1/8 -1) '-1)
(nut = (min -1 -1/2) '-1)
(nut = (min -1/2 -1) '-1)
(nut = (min -1 -1/8) '-1)
(nut = (min -1/8 -1) '-1)
(nut = (min -4.0 1.0) '-4.0)
(nut = (min 1.0 -4.0) '-4.0)
(nut = (min -4.0 4.0) '-4.0)
(nut = (min 4.0 -4.0) '-4.0)
(nut = (min -4.0 -1.0) '-4.0)
(nut = (min -1.0 -4.0) '-4.0)
(nut = (min -4.0 -4.0) '-4.0)
(nut = (min -4.0 -4.0) '-4.0)
(nut = (min -4 1/2) '-4)
(nut = (min 1/2 -4) '-4)
(nut = (min -4 1/8) '-4)
(nut = (min 1/8 -4) '-4)
(nut = (min -4 -1/2) '-4)
(nut = (min -1/2 -4) '-4)
(nut = (min -4 -1/8) '-4)
(nut = (min -1/8 -4) '-4)
(nut = (min 1.0 4.0) '1.0)
(nut = (min 4.0 1.0) '1.0)
(nut = (min 1.0 -1.0) '-1.0)
(nut = (min -1.0 1.0) '-1.0)
(nut = (min 1.0 -4.0) '-4.0)
(nut = (min -4.0 1.0) '-4.0)
(nut = (min 1.0 0.5) '0.5)
(nut = (min 0.5 1.0) '0.5)
(nut = (min 1.0 0.125) '0.125)
(nut = (min 0.125 1.0) '0.125)
(nut = (min 1.0 -0.5) '-0.5)
(nut = (min -0.5 1.0) '-0.5)
(nut = (min 1.0 -0.125) '-0.125)
(nut = (min -0.125 1.0) '-0.125)
(nut = (min 4.0 -1.0) '-1.0)
(nut = (min -1.0 4.0) '-1.0)
(nut = (min 4.0 -4.0) '-4.0)
(nut = (min -4.0 4.0) '-4.0)
(nut = (min 4.0 0.5) '0.5)
(nut = (min 0.5 4.0) '0.5)
(nut = (min 4.0 0.125) '0.125)
(nut = (min 0.125 4.0) '0.125)
(nut = (min 4.0 -0.5) '-0.5)
(nut = (min -0.5 4.0) '-0.5)
(nut = (min 4.0 -0.125) '-0.125)
(nut = (min -0.125 4.0) '-0.125)
(nut = (min -1.0 -4.0) '-4.0)
(nut = (min -4.0 -1.0) '-4.0)
(nut = (min -1.0 0.5) '-1.0)
(nut = (min 0.5 -1.0) '-1.0)
(nut = (min -1.0 0.125) '-1.0)
(nut = (min 0.125 -1.0) '-1.0)
(nut = (min -1.0 -0.5) '-1.0)
(nut = (min -0.5 -1.0) '-1.0)
(nut = (min -1.0 -0.125) '-1.0)
(nut = (min -0.125 -1.0) '-1.0)
(nut = (min -4.0 0.5) '-4.0)
(nut = (min 0.5 -4.0) '-4.0)
(nut = (min -4.0 0.125) '-4.0)
(nut = (min 0.125 -4.0) '-4.0)
(nut = (min -4.0 -0.5) '-4.0)
(nut = (min -0.5 -4.0) '-4.0)
(nut = (min -4.0 -0.125) '-4.0)
(nut = (min -0.125 -4.0) '-4.0)
(nut = (min 1/2 1/8) '1/8)
(nut = (min 1/8 1/2) '1/8)
(nut = (min 1/2 -1/2) '-1/2)
(nut = (min -1/2 1/2) '-1/2)
(nut = (min 1/2 -1/8) '-1/8)
(nut = (min -1/8 1/2) '-1/8)
(nut = (min 1/8 -1/2) '-1/2)
(nut = (min -1/2 1/8) '-1/2)
(nut = (min 1/8 -1/8) '-1/8)
(nut = (min -1/8 1/8) '-1/8)
(nut = (min -1/2 -1/8) '-1/2)
(nut = (min -1/8 -1/2) '-1/2)

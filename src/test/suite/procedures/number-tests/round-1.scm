(load (path #path ".." ".." ".." "lib.scm"))
(nut = (round 0) '0)
(nut = (round 0.0) '0.0)
(nut = (round Infinity) 'Infinity)
(nut = (round -Infinity) '-Infinity)
(nut = (round NaN) 'NaN)
(nut = (round 1) '1)
(nut = (round 4) '4)
(nut = (round -1) '-1)
(nut = (round -4) '-4)
(nut = (round 1.0) '1.0)
(nut = (round 4.0) '4.0)
(nut = (round -1.0) '-1.0)
(nut = (round -4.0) '-4.0)
(nut = (round 1/2) '1)
(nut = (round 1/8) '0)
(nut = (round -1/2) '0)
(nut = (round -1/8) '0)

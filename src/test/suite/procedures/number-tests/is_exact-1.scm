(load "/Users/jordanrandleman/Desktop/EScheme/src/test/suite/procedures/../../lib.scm")
(nut equal? (exact? 0) '#t)
(nut equal? (exact? 0.0) '#f)
(nut equal? (exact? Infinity) '#f)
(nut equal? (exact? -Infinity) '#f)
(nut equal? (exact? NaN) '#f)
(nut equal? (exact? 1) '#t)
(nut equal? (exact? 4) '#t)
(nut equal? (exact? -1) '#t)
(nut equal? (exact? -4) '#t)
(nut equal? (exact? 1.0) '#f)
(nut equal? (exact? 4.0) '#f)
(nut equal? (exact? -1.0) '#f)
(nut equal? (exact? -4.0) '#f)
(nut equal? (exact? 1/2) '#t)
(nut equal? (exact? 1/8) '#t)
(nut equal? (exact? -1/2) '#t)
(nut equal? (exact? -1/8) '#t)
(nut equal? (exact? 0+1i) '#t)
(nut equal? (exact? 0-1i) '#t)
(nut equal? (exact? 0+2i) '#t)
(nut equal? (exact? 0.0+2.0i) '#f)
(nut equal? (exact? 0+1/2i) '#t)
(nut equal? (exact? 0-2i) '#t)
(nut equal? (exact? 0.0-2.0i) '#f)
(nut equal? (exact? 0-1/2i) '#t)
(nut equal? (exact? 0.0+Infinityi) '#f)
(nut equal? (exact? 0.0-Infinityi) '#f)
(nut equal? (exact? 1+4i) '#t)
(nut equal? (exact? 2+8i) '#t)
(nut equal? (exact? 1-4i) '#t)
(nut equal? (exact? 2-8i) '#t)
(nut equal? (exact? -1+4i) '#t)
(nut equal? (exact? -2+8i) '#t)
(nut equal? (exact? -1-4i) '#t)
(nut equal? (exact? -2-8i) '#t)
(nut equal? (exact? 1.0+4.0i) '#f)
(nut equal? (exact? 2.0+8.0i) '#f)
(nut equal? (exact? 1.0-4.0i) '#f)
(nut equal? (exact? 2.0-8.0i) '#f)
(nut equal? (exact? -1.0+4.0i) '#f)
(nut equal? (exact? -2.0+8.0i) '#f)
(nut equal? (exact? -1.0-4.0i) '#f)
(nut equal? (exact? -2.0-8.0i) '#f)
(nut equal? (exact? 1/2+3/4i) '#t)
(nut equal? (exact? 3/8+5/8i) '#t)
(nut equal? (exact? 1/2-3/4i) '#t)
(nut equal? (exact? 3/8-5/8i) '#t)
(nut equal? (exact? -1/2+3/4i) '#t)
(nut equal? (exact? -3/8+5/8i) '#t)
(nut equal? (exact? -1/2-3/4i) '#t)
(nut equal? (exact? -3/8-5/8i) '#t)
(nut equal? (exact? Infinity+1.0i) '#f)
(nut equal? (exact? Infinity+4.0i) '#f)
(nut equal? (exact? Infinity-1.0i) '#f)
(nut equal? (exact? Infinity-4.0i) '#f)
(nut equal? (exact? -Infinity+1.0i) '#f)
(nut equal? (exact? -Infinity+4.0i) '#f)
(nut equal? (exact? -Infinity-1.0i) '#f)
(nut equal? (exact? -Infinity-4.0i) '#f)
(nut equal? (exact? 1.0+Infinityi) '#f)
(nut equal? (exact? 4.0+Infinityi) '#f)
(nut equal? (exact? 1.0-Infinityi) '#f)
(nut equal? (exact? 4.0-Infinityi) '#f)
(nut equal? (exact? -1.0+Infinityi) '#f)
(nut equal? (exact? -4.0+Infinityi) '#f)
(nut equal? (exact? -1.0-Infinityi) '#f)
(nut equal? (exact? -4.0-Infinityi) '#f)
(nut equal? (exact? Infinity+Infinityi) '#f)
(nut equal? (exact? -Infinity+Infinityi) '#f)
(nut equal? (exact? Infinity-Infinityi) '#f)
(nut equal? (exact? -Infinity-Infinityi) '#f)
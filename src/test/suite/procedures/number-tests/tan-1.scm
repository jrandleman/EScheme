(load (path #path ".." ".." ".." "lib.scm"))
(nut equal? (tan 0) '0)
(nut equal? (tan 0.0) '0.0)
(nut equal? (tan Infinity) 'NaN)
(nut equal? (tan -Infinity) 'NaN)
(nut equal? (tan NaN) 'NaN)
(nut equal? (tan 1) '1.557407724654902)
(nut equal? (tan 4) '1.1578212823495775)
(nut equal? (tan -1) '-1.557407724654902)
(nut equal? (tan -4) '-1.1578212823495775)
(nut equal? (tan 1.0) '1.557407724654902)
(nut equal? (tan 4.0) '1.1578212823495775)
(nut equal? (tan -1.0) '-1.557407724654902)
(nut equal? (tan -4.0) '-1.1578212823495775)
(nut equal? (tan 1/2) '0.5463024898437905)
(nut equal? (tan 1/8) '0.12565513657513097)
(nut equal? (tan -1/2) '-0.5463024898437905)
(nut equal? (tan -1/8) '-0.12565513657513097)
(nut equal? (tan 0+1i) '0.0+0.7615941559557649i)
(nut equal? (tan 0-1i) '0.0-0.7615941559557649i)
(nut equal? (tan 0+2i) '0.0+0.9640275800758169i)
(nut equal? (tan 0.0+2.0i) '0.0+0.9640275800758169i)
(nut equal? (tan 0+1/2i) '0.0+0.46211715726000974i)
(nut equal? (tan 0-2i) '0.0-0.9640275800758169i)
(nut equal? (tan 0.0-2.0i) '0.0-0.9640275800758169i)
(nut equal? (tan 0-1/2i) '0.0-0.46211715726000974i)
(nut equal? (tan 0.0+Infinityi) '0.0+1.0i)
(nut equal? (tan 0.0-Infinityi) '0.0-1.0i)
(nut equal? (tan 1+4i) '6.102409213762599e-4+1.0002790562344657i)
(nut equal? (tan 2+8i) '-1.7033382713369272e-7+1.0000001471157944i)
(nut equal? (tan 1-4i) '6.102409213762599e-4-1.0002790562344657i)
(nut equal? (tan 2-8i) '-1.7033382713369272e-7-1.0000001471157944i)
(nut equal? (tan -1+4i) '-6.102409213762599e-4+1.0002790562344657i)
(nut equal? (tan -2+8i) '1.7033382713369272e-7+1.0000001471157944i)
(nut equal? (tan -1-4i) '-6.102409213762599e-4-1.0002790562344657i)
(nut equal? (tan -2-8i) '1.7033382713369272e-7-1.0000001471157944i)
(nut equal? (tan 1.0+4.0i) '6.102409213762599e-4+1.0002790562344657i)
(nut equal? (tan 2.0+8.0i) '-1.7033382713369272e-7+1.0000001471157944i)
(nut equal? (tan 1.0-4.0i) '6.102409213762599e-4-1.0002790562344657i)
(nut equal? (tan 2.0-8.0i) '-1.7033382713369272e-7-1.0000001471157944i)
(nut equal? (tan -1.0+4.0i) '-6.102409213762599e-4+1.0002790562344657i)
(nut equal? (tan -2.0+8.0i) '1.7033382713369272e-7+1.0000001471157944i)
(nut equal? (tan -1.0-4.0i) '-6.102409213762599e-4-1.0002790562344657i)
(nut equal? (tan -2.0-8.0i) '1.7033382713369272e-7-1.0000001471157944i)
(nut equal? (tan 1/2+3/4i) '0.2908934618296181+0.7360841705511909i)
(nut equal? (tan 3/8+5/8i) '0.26015627039519434+0.6113931863142529i)
(nut equal? (tan 1/2-3/4i) '0.2908934618296181-0.7360841705511909i)
(nut equal? (tan 3/8-5/8i) '0.26015627039519434-0.6113931863142529i)
(nut equal? (tan -1/2+3/4i) '-0.2908934618296181+0.7360841705511909i)
(nut equal? (tan -3/8+5/8i) '-0.26015627039519434+0.6113931863142529i)
(nut equal? (tan -1/2-3/4i) '-0.2908934618296181-0.7360841705511909i)
(nut equal? (tan -3/8-5/8i) '-0.26015627039519434-0.6113931863142529i)
(nut equal? (tan Infinity+1.0i) 'NaN)
(nut equal? (tan Infinity+4.0i) 'NaN)
(nut equal? (tan Infinity-1.0i) 'NaN)
(nut equal? (tan Infinity-4.0i) 'NaN)
(nut equal? (tan -Infinity+1.0i) 'NaN)
(nut equal? (tan -Infinity+4.0i) 'NaN)
(nut equal? (tan -Infinity-1.0i) 'NaN)
(nut equal? (tan -Infinity-4.0i) 'NaN)
(nut equal? (tan 1.0+Infinityi) '0.0+1.0i)
(nut equal? (tan 4.0+Infinityi) '0.0+1.0i)
(nut equal? (tan 1.0-Infinityi) '0.0-1.0i)
(nut equal? (tan 4.0-Infinityi) '0.0-1.0i)
(nut equal? (tan -1.0+Infinityi) '-0.0+1.0i)
(nut equal? (tan -4.0+Infinityi) '-0.0+1.0i)
(nut equal? (tan -1.0-Infinityi) '-0.0-1.0i)
(nut equal? (tan -4.0-Infinityi) '-0.0-1.0i)
(nut equal? (tan Infinity+Infinityi) '0.0+1.0i)
(nut equal? (tan -Infinity+Infinityi) '-0.0+1.0i)
(nut equal? (tan Infinity-Infinityi) '0.0-1.0i)
(nut equal? (tan -Infinity-Infinityi) '-0.0-1.0i)

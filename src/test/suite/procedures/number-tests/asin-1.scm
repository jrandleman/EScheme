(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (asin 0) '0)
(nut equal? (asin 0.0) '0.0)
(nut equal? (asin Infinity) 'NaN)
(nut equal? (asin -Infinity) 'NaN)
(nut equal? (asin NaN) 'NaN)
(nut equal? (asin 1) '1.5707963267948966)
(nut equal? (asin 4) '1.5707963267948966-2.0634370688955608i)
(nut equal? (asin -1) '-1.5707963267948966)
(nut equal? (asin -4) '-1.5707963267948966+2.0634370688955608i)
(nut equal? (asin 1.0) '1.5707963267948966)
(nut equal? (asin 4.0) '1.5707963267948966-2.0634370688955608i)
(nut equal? (asin -1.0) '-1.5707963267948966)
(nut equal? (asin -4.0) '-1.5707963267948966+2.0634370688955608i)
(nut equal? (asin 1/2) '0.5235987755982988)
(nut equal? (asin 1/8) '0.1253278311680654)
(nut equal? (asin -1/2) '-0.5235987755982988)
(nut equal? (asin -1/8) '-0.1253278311680654)
(nut equal? (asin 0+1i) '0.0+0.881373587019543i)
(nut equal? (asin 0-1i) '0.0-0.881373587019543i)
(nut equal? (asin 0+2i) '0.0+1.4436354751788103i)
(nut equal? (asin 0.0+2.0i) '0.0+1.4436354751788103i)
(nut equal? (asin 0+1/2i) '0.0+0.48121182505960347i)
(nut equal? (asin 0-2i) '0.0-1.4436354751788103i)
(nut equal? (asin 0.0-2.0i) '0.0-1.4436354751788103i)
(nut equal? (asin 0-1/2i) '0.0-0.48121182505960347i)
(nut equal? (asin 0.0+Infinityi) '0.0+Infinityi)
(nut equal? (asin 0.0-Infinityi) '0.0-Infinityi)
(nut equal? (asin 1+4i) '0.2383174618098661+2.122550123810071i)
(nut equal? (asin 2+8i) '0.24326523307216896+2.8061337001907227i)
(nut equal? (asin 1-4i) '0.2383174618098661-2.122550123810071i)
(nut equal? (asin 2-8i) '0.24326523307216896-2.8061337001907227i)
(nut equal? (asin -1+4i) '-0.2383174618098661+2.122550123810071i)
(nut equal? (asin -2+8i) '-0.24326523307216896+2.8061337001907227i)
(nut equal? (asin -1-4i) '-0.2383174618098661-2.122550123810071i)
(nut equal? (asin -2-8i) '-0.24326523307216896-2.8061337001907227i)
(nut equal? (asin 1.0+4.0i) '0.2383174618098661+2.122550123810071i)
(nut equal? (asin 2.0+8.0i) '0.24326523307216896+2.8061337001907227i)
(nut equal? (asin 1.0-4.0i) '0.2383174618098661-2.122550123810071i)
(nut equal? (asin 2.0-8.0i) '0.24326523307216896-2.8061337001907227i)
(nut equal? (asin -1.0+4.0i) '-0.2383174618098661+2.122550123810071i)
(nut equal? (asin -2.0+8.0i) '-0.24326523307216896+2.8061337001907227i)
(nut equal? (asin -1.0-4.0i) '-0.2383174618098661-2.122550123810071i)
(nut equal? (asin -2.0-8.0i) '-0.24326523307216896-2.8061337001907227i)
(nut equal? (asin 1/2+3/4i) '0.3982778735383084+0.7433204263252785i)
(nut equal? (asin 3/8+5/8i) '0.31861191138228206+0.6180219712442493i)
(nut equal? (asin 1/2-3/4i) '0.3982778735383084-0.7433204263252785i)
(nut equal? (asin 3/8-5/8i) '0.31861191138228206-0.6180219712442493i)
(nut equal? (asin -1/2+3/4i) '-0.3982778735383084+0.7433204263252785i)
(nut equal? (asin -3/8+5/8i) '-0.31861191138228206+0.6180219712442493i)
(nut equal? (asin -1/2-3/4i) '-0.3982778735383084-0.7433204263252785i)
(nut equal? (asin -3/8-5/8i) '-0.31861191138228206-0.6180219712442493i)
(nut equal? (asin Infinity+1.0i) 'NaN)
(nut equal? (asin Infinity+4.0i) 'NaN)
(nut equal? (asin Infinity-1.0i) 'NaN)
(nut equal? (asin Infinity-4.0i) 'NaN)
(nut equal? (asin -Infinity+1.0i) 'NaN)
(nut equal? (asin -Infinity+4.0i) 'NaN)
(nut equal? (asin -Infinity-1.0i) 'NaN)
(nut equal? (asin -Infinity-4.0i) 'NaN)
(nut equal? (asin 1.0+Infinityi) '0.0+Infinityi)
(nut equal? (asin 4.0+Infinityi) '0.0+Infinityi)
(nut equal? (asin 1.0-Infinityi) '0.0-Infinityi)
(nut equal? (asin 4.0-Infinityi) '0.0-Infinityi)
(nut equal? (asin -1.0+Infinityi) '-0.0+Infinityi)
(nut equal? (asin -4.0+Infinityi) '-0.0+Infinityi)
(nut equal? (asin -1.0-Infinityi) '-0.0-Infinityi)
(nut equal? (asin -4.0-Infinityi) '-0.0-Infinityi)
(nut equal? (asin Infinity+Infinityi) 'NaN)
(nut equal? (asin -Infinity+Infinityi) 'NaN)
(nut equal? (asin Infinity-Infinityi) 'NaN)
(nut equal? (asin -Infinity-Infinityi) 'NaN)

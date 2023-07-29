(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (tanh 0) '0)
(nut equal? (tanh 0.0) '0.0)
(nut equal? (tanh Infinity) '1.0)
(nut equal? (tanh -Infinity) '-1.0)
(nut equal? (tanh NaN) 'NaN)
(nut equal? (tanh 1) '0.7615941559557649)
(nut equal? (tanh 4) '0.999329299739067)
(nut equal? (tanh -1) '-0.7615941559557649)
(nut equal? (tanh -4) '-0.999329299739067)
(nut equal? (tanh 1.0) '0.7615941559557649)
(nut equal? (tanh 4.0) '0.999329299739067)
(nut equal? (tanh -1.0) '-0.7615941559557649)
(nut equal? (tanh -4.0) '-0.999329299739067)
(nut equal? (tanh 1/2) '0.46211715726000974)
(nut equal? (tanh 1/8) '0.12435300177159621)
(nut equal? (tanh -1/2) '-0.46211715726000974)
(nut equal? (tanh -1/8) '-0.12435300177159621)
(nut equal? (tanh 0+1i) '0.0+1.557407724654902i)
(nut equal? (tanh 0-1i) '0.0-1.557407724654902i)
(nut equal? (tanh 0+2i) '0.0-2.185039863261519i)
(nut equal? (tanh 0.0+2.0i) '0.0-2.185039863261519i)
(nut equal? (tanh 0+1/2i) '0.0+0.5463024898437905i)
(nut equal? (tanh 0-2i) '0.0+2.185039863261519i)
(nut equal? (tanh 0.0-2.0i) '0.0+2.185039863261519i)
(nut equal? (tanh 0-1/2i) '0.0-0.5463024898437905i)
(nut equal? (tanh 0.0+Infinityi) 'NaN)
(nut equal? (tanh 0.0-Infinityi) 'NaN)
(nut equal? (tanh 1+4i) '1.002810507583505+0.2735530828073074i)
(nut equal? (tanh 2+8i) '1.0356479469632376-0.010925884335752534i)
(nut equal? (tanh 1-4i) '1.002810507583505-0.2735530828073074i)
(nut equal? (tanh 2-8i) '1.0356479469632376+0.010925884335752534i)
(nut equal? (tanh -1+4i) '-1.002810507583505+0.2735530828073074i)
(nut equal? (tanh -2+8i) '-1.0356479469632376-0.010925884335752534i)
(nut equal? (tanh -1-4i) '-1.002810507583505-0.2735530828073074i)
(nut equal? (tanh -2-8i) '-1.0356479469632376+0.010925884335752534i)
(nut equal? (tanh 1.0+4.0i) '1.002810507583505+0.2735530828073074i)
(nut equal? (tanh 2.0+8.0i) '1.0356479469632376-0.010925884335752534i)
(nut equal? (tanh 1.0-4.0i) '1.002810507583505-0.2735530828073074i)
(nut equal? (tanh 2.0-8.0i) '1.0356479469632376+0.010925884335752534i)
(nut equal? (tanh -1.0+4.0i) '-1.002810507583505+0.2735530828073074i)
(nut equal? (tanh -2.0+8.0i) '-1.0356479469632376-0.010925884335752534i)
(nut equal? (tanh -1.0-4.0i) '-1.002810507583505-0.2735530828073074i)
(nut equal? (tanh -2.0-8.0i) '-1.0356479469632376+0.010925884335752534i)
(nut equal? (tanh 1/2+3/4i) '0.7282118012804726+0.6180963948062023i)
(nut equal? (tanh 3/8+5/8i) '0.5107539426530953+0.5894293731710623i)
(nut equal? (tanh 1/2-3/4i) '0.7282118012804726-0.6180963948062023i)
(nut equal? (tanh 3/8-5/8i) '0.5107539426530953-0.5894293731710623i)
(nut equal? (tanh -1/2+3/4i) '-0.7282118012804726+0.6180963948062023i)
(nut equal? (tanh -3/8+5/8i) '-0.5107539426530953+0.5894293731710623i)
(nut equal? (tanh -1/2-3/4i) '-0.7282118012804726-0.6180963948062023i)
(nut equal? (tanh -3/8-5/8i) '-0.5107539426530953-0.5894293731710623i)
(nut equal? (tanh Infinity+1.0i) '1.0+0.0i)
(nut equal? (tanh Infinity+4.0i) '1.0+0.0i)
(nut equal? (tanh Infinity-1.0i) '1.0-0.0i)
(nut equal? (tanh Infinity-4.0i) '1.0-0.0i)
(nut equal? (tanh -Infinity+1.0i) '-1.0+0.0i)
(nut equal? (tanh -Infinity+4.0i) '-1.0+0.0i)
(nut equal? (tanh -Infinity-1.0i) '-1.0-0.0i)
(nut equal? (tanh -Infinity-4.0i) '-1.0-0.0i)
(nut equal? (tanh 1.0+Infinityi) 'NaN)
(nut equal? (tanh 4.0+Infinityi) 'NaN)
(nut equal? (tanh 1.0-Infinityi) 'NaN)
(nut equal? (tanh 4.0-Infinityi) 'NaN)
(nut equal? (tanh -1.0+Infinityi) 'NaN)
(nut equal? (tanh -4.0+Infinityi) 'NaN)
(nut equal? (tanh -1.0-Infinityi) 'NaN)
(nut equal? (tanh -4.0-Infinityi) 'NaN)
(nut equal? (tanh Infinity+Infinityi) '1.0+0.0i)
(nut equal? (tanh -Infinity+Infinityi) '-1.0+0.0i)
(nut equal? (tanh Infinity-Infinityi) '1.0-0.0i)
(nut equal? (tanh -Infinity-Infinityi) '-1.0-0.0i)

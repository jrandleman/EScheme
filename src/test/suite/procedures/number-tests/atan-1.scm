(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (atan 0) '0)
(nut equal? (atan 0.0) '0.0)
(nut equal? (atan Infinity) '1.5707963267948966)
(nut equal? (atan -Infinity) '-1.5707963267948966)
(nut equal? (atan NaN) 'NaN)
(nut equal? (atan 1) '0.7853981633974483)
(nut equal? (atan 4) '1.3258176636680326)
(nut equal? (atan -1) '-0.7853981633974483)
(nut equal? (atan -4) '-1.3258176636680326)
(nut equal? (atan 1.0) '0.7853981633974483)
(nut equal? (atan 4.0) '1.3258176636680326)
(nut equal? (atan -1.0) '-0.7853981633974483)
(nut equal? (atan -4.0) '-1.3258176636680326)
(nut equal? (atan 1/2) '0.46364760900080615)
(nut equal? (atan 1/8) '0.12435499454676144)
(nut equal? (atan -1/2) '-0.46364760900080615)
(nut equal? (atan -1/8) '-0.12435499454676144)
:nop
:nop
(nut equal? (atan 0+2i) '1.5707963267948966+0.5493061443340549i)
(nut equal? (atan 0.0+2.0i) '1.5707963267948966+0.5493061443340549i)
(nut equal? (atan 0+1/2i) '0.0+0.5493061443340549i)
(nut equal? (atan 0-2i) '1.5707963267948966-0.5493061443340549i)
(nut equal? (atan 0.0-2.0i) '1.5707963267948966-0.5493061443340549i)
(nut equal? (atan 0-1/2i) '0.0-0.5493061443340549i)
(nut equal? (atan 0.0+Infinityi) '1.5707963267948966+0.0i)
(nut equal? (atan 0.0-Infinityi) '1.5707963267948966-0.0i)
(nut equal? (atan 1+4i) '1.508618829521516+0.2388778612568591i)
(nut equal? (atan 2+8i) '1.540980970229312+0.11808983573454863i)
(nut equal? (atan 1-4i) '1.508618829521516-0.2388778612568591i)
(nut equal? (atan 2-8i) '1.540980970229312-0.11808983573454863i)
(nut equal? (atan -1+4i) '-1.508618829521516+0.2388778612568591i)
(nut equal? (atan -2+8i) '-1.540980970229312+0.11808983573454863i)
(nut equal? (atan -1-4i) '-1.508618829521516-0.2388778612568591i)
(nut equal? (atan -2-8i) '-1.540980970229312-0.11808983573454863i)
(nut equal? (atan 1.0+4.0i) '1.508618829521516+0.2388778612568591i)
(nut equal? (atan 2.0+8.0i) '1.540980970229312+0.11808983573454863i)
(nut equal? (atan 1.0-4.0i) '1.508618829521516-0.2388778612568591i)
(nut equal? (atan 2.0-8.0i) '1.540980970229312-0.11808983573454863i)
(nut equal? (atan -1.0+4.0i) '-1.508618829521516+0.2388778612568591i)
(nut equal? (atan -2.0+8.0i) '-1.540980970229312+0.11808983573454863i)
(nut equal? (atan -1.0-4.0i) '-1.508618829521516-0.2388778612568591i)
(nut equal? (atan -2.0-8.0i) '-1.540980970229312-0.11808983573454863i)
(nut equal? (atan 1/2+3/4i) '0.6927241883996009+0.5902135002795054i)
(nut equal? (atan 3/8+5/8i) '0.5060985057256671+0.5728529480989801i)
(nut equal? (atan 1/2-3/4i) '0.6927241883996009-0.5902135002795054i)
(nut equal? (atan 3/8-5/8i) '0.5060985057256671-0.5728529480989801i)
(nut equal? (atan -1/2+3/4i) '-0.6927241883996009+0.5902135002795054i)
(nut equal? (atan -3/8+5/8i) '-0.5060985057256671+0.5728529480989801i)
(nut equal? (atan -1/2-3/4i) '-0.6927241883996009-0.5902135002795054i)
(nut equal? (atan -3/8-5/8i) '-0.5060985057256671-0.5728529480989801i)
(nut equal? (atan Infinity+1.0i) 'NaN)
(nut equal? (atan Infinity+4.0i) 'NaN)
(nut equal? (atan Infinity-1.0i) 'NaN)
(nut equal? (atan Infinity-4.0i) 'NaN)
(nut equal? (atan -Infinity+1.0i) 'NaN)
(nut equal? (atan -Infinity+4.0i) 'NaN)
(nut equal? (atan -Infinity-1.0i) 'NaN)
(nut equal? (atan -Infinity-4.0i) 'NaN)
(nut equal? (atan 1.0+Infinityi) '1.5707963267948966+0.0i)
(nut equal? (atan 4.0+Infinityi) '1.5707963267948966+0.0i)
(nut equal? (atan 1.0-Infinityi) '1.5707963267948966-0.0i)
(nut equal? (atan 4.0-Infinityi) '1.5707963267948966-0.0i)
(nut equal? (atan -1.0+Infinityi) '-1.5707963267948966+0.0i)
(nut equal? (atan -4.0+Infinityi) '-1.5707963267948966+0.0i)
(nut equal? (atan -1.0-Infinityi) '-1.5707963267948966-0.0i)
(nut equal? (atan -4.0-Infinityi) '-1.5707963267948966-0.0i)
(nut equal? (atan Infinity+Infinityi) '1.5707963267948966+0.0i)
(nut equal? (atan -Infinity+Infinityi) '-1.5707963267948966+0.0i)
(nut equal? (atan Infinity-Infinityi) '1.5707963267948966-0.0i)
(nut equal? (atan -Infinity-Infinityi) '-1.5707963267948966-0.0i)

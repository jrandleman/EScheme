(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (sin 0) '0)
(nut equal? (sin 0.0) '0.0)
(nut equal? (sin Infinity) 'NaN)
(nut equal? (sin -Infinity) 'NaN)
(nut equal? (sin NaN) 'NaN)
(nut equal? (sin 1) '0.8414709848078965)
(nut equal? (sin 4) '-0.7568024953079282)
(nut equal? (sin -1) '-0.8414709848078965)
(nut equal? (sin -4) '0.7568024953079282)
(nut equal? (sin 1.0) '0.8414709848078965)
(nut equal? (sin 4.0) '-0.7568024953079282)
(nut equal? (sin -1.0) '-0.8414709848078965)
(nut equal? (sin -4.0) '0.7568024953079282)
(nut equal? (sin 1/2) '0.479425538604203)
(nut equal? (sin 1/8) '0.12467473338522769)
(nut equal? (sin -1/2) '-0.479425538604203)
(nut equal? (sin -1/8) '-0.12467473338522769)
(nut equal? (sin 0+1i) '0.0+1.1752011936438014i)
(nut equal? (sin 0-1i) '0.0-1.1752011936438014i)
(nut equal? (sin 0+2i) '0.0+3.6268604078470186i)
(nut equal? (sin 0.0+2.0i) '0.0+3.6268604078470186i)
(nut equal? (sin 0+1/2i) '0.0+0.5210953054937474i)
(nut equal? (sin 0-2i) '0.0-3.6268604078470186i)
(nut equal? (sin 0.0-2.0i) '0.0-3.6268604078470186i)
(nut equal? (sin 0-1/2i) '0.0-0.5210953054937474i)
(nut equal? (sin 0.0+Infinityi) '0.0+Infinityi)
(nut equal? (sin 0.0-Infinityi) '0.0-Infinityi)
(nut equal? (sin 1+4i) '22.979085577886128+14.744805188558727i)
(nut equal? (sin 2+8i) '1355.2888660639057-620.2580482928207i)
(nut equal? (sin 1-4i) '22.979085577886128-14.744805188558727i)
(nut equal? (sin 2-8i) '1355.2888660639057+620.2580482928207i)
(nut equal? (sin -1+4i) '-22.979085577886128+14.744805188558727i)
(nut equal? (sin -2+8i) '-1355.2888660639057-620.2580482928207i)
(nut equal? (sin -1-4i) '-22.979085577886128-14.744805188558727i)
(nut equal? (sin -2-8i) '-1355.2888660639057+620.2580482928207i)
(nut equal? (sin 1.0+4.0i) '22.979085577886128+14.744805188558727i)
(nut equal? (sin 2.0+8.0i) '1355.2888660639057-620.2580482928207i)
(nut equal? (sin 1.0-4.0i) '22.979085577886128-14.744805188558727i)
(nut equal? (sin 2.0-8.0i) '1355.2888660639057+620.2580482928207i)
(nut equal? (sin -1.0+4.0i) '-22.979085577886128+14.744805188558727i)
(nut equal? (sin -2.0+8.0i) '-1355.2888660639057-620.2580482928207i)
(nut equal? (sin -1.0-4.0i) '-22.979085577886128-14.744805188558727i)
(nut equal? (sin -2.0-8.0i) '-1355.2888660639057+620.2580482928207i)
(nut equal? (sin 1/2+3/4i) '0.6207042310780551+0.7216508242975646i)
(nut equal? (sin 3/8+5/8i) '0.44016936446467286+0.6201761320224791i)
(nut equal? (sin 1/2-3/4i) '0.6207042310780551-0.7216508242975646i)
(nut equal? (sin 3/8-5/8i) '0.44016936446467286-0.6201761320224791i)
(nut equal? (sin -1/2+3/4i) '-0.6207042310780551+0.7216508242975646i)
(nut equal? (sin -3/8+5/8i) '-0.44016936446467286+0.6201761320224791i)
(nut equal? (sin -1/2-3/4i) '-0.6207042310780551-0.7216508242975646i)
(nut equal? (sin -3/8-5/8i) '-0.44016936446467286-0.6201761320224791i)
(nut equal? (sin Infinity+1.0i) 'NaN)
(nut equal? (sin Infinity+4.0i) 'NaN)
(nut equal? (sin Infinity-1.0i) 'NaN)
(nut equal? (sin Infinity-4.0i) 'NaN)
(nut equal? (sin -Infinity+1.0i) 'NaN)
(nut equal? (sin -Infinity+4.0i) 'NaN)
(nut equal? (sin -Infinity-1.0i) 'NaN)
(nut equal? (sin -Infinity-4.0i) 'NaN)
(nut equal? (sin 1.0+Infinityi) 'Infinity+Infinityi)
(nut equal? (sin 4.0+Infinityi) '-Infinity-Infinityi)
(nut equal? (sin 1.0-Infinityi) 'Infinity-Infinityi)
(nut equal? (sin 4.0-Infinityi) '-Infinity+Infinityi)
(nut equal? (sin -1.0+Infinityi) '-Infinity+Infinityi)
(nut equal? (sin -4.0+Infinityi) 'Infinity-Infinityi)
(nut equal? (sin -1.0-Infinityi) '-Infinity-Infinityi)
(nut equal? (sin -4.0-Infinityi) 'Infinity+Infinityi)
(nut equal? (sin Infinity+Infinityi) 'NaN)
(nut equal? (sin -Infinity+Infinityi) 'NaN)
(nut equal? (sin Infinity-Infinityi) 'NaN)
(nut equal? (sin -Infinity-Infinityi) 'NaN)

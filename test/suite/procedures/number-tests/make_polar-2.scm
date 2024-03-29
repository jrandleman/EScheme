(load (path (path-parent #path 3) "lib.scm"))
(nut = (make-polar 0.0 0.0) '0.0+0.0i)
(nut = (make-polar 0.0 0.0) '0.0+0.0i)
(nut = (make-polar 0.0 Infinity) 'NaN)
(nut = (make-polar Infinity 0.0) 'NaN)
(nut = (make-polar 0.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 0.0) 'NaN)
(nut = (make-polar 0.0 NaN) 'NaN)
(nut = (make-polar NaN 0.0) 'NaN)
(nut = (make-polar 0 1) '0)
(nut = (make-polar 1 0) '1)
(nut = (make-polar 0 4) '0)
(nut = (make-polar 4 0) '4)
(nut = (make-polar 0 -1) '0)
(nut = (make-polar -1 0) '-1)
(nut = (make-polar 0 -4) '0)
(nut = (make-polar -4 0) '-4)
(nut = (make-polar 0.0 1.0) '0.0+0.0i)
(nut = (make-polar 1.0 0.0) '1.0+0.0i)
(nut = (make-polar 0.0 4.0) '-0.0-0.0i)
(nut = (make-polar 4.0 0.0) '4.0+0.0i)
(nut = (make-polar 0.0 -1.0) '0.0-0.0i)
(nut = (make-polar -1.0 0.0) '-1.0-0.0i)
(nut = (make-polar 0.0 -4.0) '-0.0+0.0i)
(nut = (make-polar -4.0 0.0) '-4.0-0.0i)
(nut = (make-polar 0 1/2) '0)
(nut = (make-polar 1/2 0) '1/2)
(nut = (make-polar 0 1/8) '0)
(nut = (make-polar 1/8 0) '1/8)
(nut = (make-polar 0 -1/2) '0)
(nut = (make-polar -1/2 0) '-1/2)
(nut = (make-polar 0 -1/8) '0)
(nut = (make-polar -1/8 0) '-1/8)
(nut = (make-polar 0.0 Infinity) 'NaN)
(nut = (make-polar Infinity 0.0) 'NaN)
(nut = (make-polar 0.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 0.0) 'NaN)
(nut = (make-polar 0.0 NaN) 'NaN)
(nut = (make-polar NaN 0.0) 'NaN)
(nut = (make-polar 0.0 1.0) '0.0+0.0i)
(nut = (make-polar 1.0 0.0) '1.0+0.0i)
(nut = (make-polar 0.0 4.0) '-0.0-0.0i)
(nut = (make-polar 4.0 0.0) '4.0+0.0i)
(nut = (make-polar 0.0 -1.0) '0.0-0.0i)
(nut = (make-polar -1.0 0.0) '-1.0-0.0i)
(nut = (make-polar 0.0 -4.0) '-0.0+0.0i)
(nut = (make-polar -4.0 0.0) '-4.0-0.0i)
(nut = (make-polar 0.0 1.0) '0.0+0.0i)
(nut = (make-polar 1.0 0.0) '1.0+0.0i)
(nut = (make-polar 0.0 4.0) '-0.0-0.0i)
(nut = (make-polar 4.0 0.0) '4.0+0.0i)
(nut = (make-polar 0.0 -1.0) '0.0-0.0i)
(nut = (make-polar -1.0 0.0) '-1.0-0.0i)
(nut = (make-polar 0.0 -4.0) '-0.0+0.0i)
(nut = (make-polar -4.0 0.0) '-4.0-0.0i)
(nut = (make-polar 0.0 0.5) '0.0+0.0i)
(nut = (make-polar 0.5 0.0) '0.5+0.0i)
(nut = (make-polar 0.0 0.125) '0.0+0.0i)
(nut = (make-polar 0.125 0.0) '0.125+0.0i)
(nut = (make-polar 0.0 -0.5) '0.0-0.0i)
(nut = (make-polar -0.5 0.0) '-0.5-0.0i)
(nut = (make-polar 0.0 -0.125) '0.0-0.0i)
(nut = (make-polar -0.125 0.0) '-0.125-0.0i)
(nut = (make-polar Infinity -Infinity) 'NaN)
(nut = (make-polar -Infinity Infinity) 'NaN)
(nut = (make-polar Infinity NaN) 'NaN)
(nut = (make-polar NaN Infinity) 'NaN)
(nut = (make-polar Infinity 1.0) 'Infinity+Infinityi)
(nut = (make-polar 1.0 Infinity) 'NaN)
(nut = (make-polar Infinity 4.0) '-Infinity-Infinityi)
(nut = (make-polar 4.0 Infinity) 'NaN)
(nut = (make-polar Infinity -1.0) 'Infinity-Infinityi)
(nut = (make-polar -1.0 Infinity) 'NaN)
(nut = (make-polar Infinity -4.0) '-Infinity+Infinityi)
(nut = (make-polar -4.0 Infinity) 'NaN)
(nut = (make-polar Infinity 1.0) 'Infinity+Infinityi)
(nut = (make-polar 1.0 Infinity) 'NaN)
(nut = (make-polar Infinity 4.0) '-Infinity-Infinityi)
(nut = (make-polar 4.0 Infinity) 'NaN)
(nut = (make-polar Infinity -1.0) 'Infinity-Infinityi)
(nut = (make-polar -1.0 Infinity) 'NaN)
(nut = (make-polar Infinity -4.0) '-Infinity+Infinityi)
(nut = (make-polar -4.0 Infinity) 'NaN)
(nut = (make-polar Infinity 0.5) 'Infinity+Infinityi)
(nut = (make-polar 0.5 Infinity) 'NaN)
(nut = (make-polar Infinity 0.125) 'Infinity+Infinityi)
(nut = (make-polar 0.125 Infinity) 'NaN)
(nut = (make-polar Infinity -0.5) 'Infinity-Infinityi)
(nut = (make-polar -0.5 Infinity) 'NaN)
(nut = (make-polar Infinity -0.125) 'Infinity-Infinityi)
(nut = (make-polar -0.125 Infinity) 'NaN)
(nut = (make-polar -Infinity NaN) 'NaN)
(nut = (make-polar NaN -Infinity) 'NaN)
(nut = (make-polar -Infinity 1.0) '-Infinity-Infinityi)
(nut = (make-polar 1.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 4.0) 'Infinity+Infinityi)
(nut = (make-polar 4.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity -1.0) '-Infinity+Infinityi)
(nut = (make-polar -1.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity -4.0) 'Infinity-Infinityi)
(nut = (make-polar -4.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 1.0) '-Infinity-Infinityi)
(nut = (make-polar 1.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 4.0) 'Infinity+Infinityi)
(nut = (make-polar 4.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity -1.0) '-Infinity+Infinityi)
(nut = (make-polar -1.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity -4.0) 'Infinity-Infinityi)
(nut = (make-polar -4.0 -Infinity) 'NaN)
(nut = (make-polar -Infinity 0.5) '-Infinity-Infinityi)
(nut = (make-polar 0.5 -Infinity) 'NaN)
(nut = (make-polar -Infinity 0.125) '-Infinity-Infinityi)
(nut = (make-polar 0.125 -Infinity) 'NaN)
(nut = (make-polar -Infinity -0.5) '-Infinity+Infinityi)
(nut = (make-polar -0.5 -Infinity) 'NaN)
(nut = (make-polar -Infinity -0.125) '-Infinity+Infinityi)
(nut = (make-polar -0.125 -Infinity) 'NaN)
(nut = (make-polar NaN 1.0) 'NaN)
(nut = (make-polar 1.0 NaN) 'NaN)
(nut = (make-polar NaN 4.0) 'NaN)
(nut = (make-polar 4.0 NaN) 'NaN)
(nut = (make-polar NaN -1.0) 'NaN)
(nut = (make-polar -1.0 NaN) 'NaN)
(nut = (make-polar NaN -4.0) 'NaN)
(nut = (make-polar -4.0 NaN) 'NaN)
(nut = (make-polar NaN 1.0) 'NaN)
(nut = (make-polar 1.0 NaN) 'NaN)
(nut = (make-polar NaN 4.0) 'NaN)
(nut = (make-polar 4.0 NaN) 'NaN)
(nut = (make-polar NaN -1.0) 'NaN)
(nut = (make-polar -1.0 NaN) 'NaN)
(nut = (make-polar NaN -4.0) 'NaN)
(nut = (make-polar -4.0 NaN) 'NaN)
(nut = (make-polar NaN 0.5) 'NaN)
(nut = (make-polar 0.5 NaN) 'NaN)
(nut = (make-polar NaN 0.125) 'NaN)
(nut = (make-polar 0.125 NaN) 'NaN)
(nut = (make-polar NaN -0.5) 'NaN)
(nut = (make-polar -0.5 NaN) 'NaN)
(nut = (make-polar NaN -0.125) 'NaN)
(nut = (make-polar -0.125 NaN) 'NaN)
(nut = (make-polar 1 4) '-0.6536436208636119-0.7568024953079282i)
(nut = (make-polar 4 1) '2.161209223472559+3.365883939231586i)
(nut = (make-polar 1 -1) '0.5403023058681398-0.8414709848078965i)
(nut = (make-polar -1 1) '-0.5403023058681398-0.8414709848078965i)
(nut = (make-polar 1 -4) '-0.6536436208636119+0.7568024953079282i)
(nut = (make-polar -4 1) '-2.161209223472559-3.365883939231586i)
(nut = (make-polar 1.0 1.0) '0.5403023058681398+0.8414709848078965i)
(nut = (make-polar 1.0 1.0) '0.5403023058681398+0.8414709848078965i)
(nut = (make-polar 1.0 4.0) '-0.6536436208636119-0.7568024953079282i)
(nut = (make-polar 4.0 1.0) '2.161209223472559+3.365883939231586i)
(nut = (make-polar 1.0 -1.0) '0.5403023058681398-0.8414709848078965i)
(nut = (make-polar -1.0 1.0) '-0.5403023058681398-0.8414709848078965i)
(nut = (make-polar 1.0 -4.0) '-0.6536436208636119+0.7568024953079282i)
(nut = (make-polar -4.0 1.0) '-2.161209223472559-3.365883939231586i)
(nut = (make-polar 1 1/2) '0.8775825618903728+0.479425538604203i)
(nut = (make-polar 1/2 1) '0.2701511529340699+0.42073549240394825i)
(nut = (make-polar 1 1/8) '0.992197667229329+0.12467473338522769i)
(nut = (make-polar 1/8 1) '0.06753778823351747+0.10518387310098706i)
(nut = (make-polar 1 -1/2) '0.8775825618903728-0.479425538604203i)
(nut = (make-polar -1/2 1) '-0.2701511529340699-0.42073549240394825i)
(nut = (make-polar 1 -1/8) '0.992197667229329-0.12467473338522769i)
(nut = (make-polar -1/8 1) '-0.06753778823351747-0.10518387310098706i)
(nut = (make-polar 4 -1) '2.161209223472559-3.365883939231586i)
(nut = (make-polar -1 4) '0.6536436208636119+0.7568024953079282i)
(nut = (make-polar 4 -4) '-2.6145744834544478+3.027209981231713i)
(nut = (make-polar -4 4) '2.6145744834544478+3.027209981231713i)
(nut = (make-polar 4.0 1.0) '2.161209223472559+3.365883939231586i)
(nut = (make-polar 1.0 4.0) '-0.6536436208636119-0.7568024953079282i)
(nut = (make-polar 4.0 4.0) '-2.6145744834544478-3.027209981231713i)
(nut = (make-polar 4.0 4.0) '-2.6145744834544478-3.027209981231713i)
(nut = (make-polar 4.0 -1.0) '2.161209223472559-3.365883939231586i)
(nut = (make-polar -1.0 4.0) '0.6536436208636119+0.7568024953079282i)
(nut = (make-polar 4.0 -4.0) '-2.6145744834544478+3.027209981231713i)
(nut = (make-polar -4.0 4.0) '2.6145744834544478+3.027209981231713i)
(nut = (make-polar 4 1/2) '3.510330247561491+1.917702154416812i)
(nut = (make-polar 1/2 4) '-0.32682181043180597-0.3784012476539641i)
(nut = (make-polar 4 1/8) '3.968790668917316+0.49869893354091077i)
(nut = (make-polar 1/8 4) '-0.08170545260795149-0.09460031191349103i)
(nut = (make-polar 4 -1/2) '3.510330247561491-1.917702154416812i)
(nut = (make-polar -1/2 4) '0.32682181043180597+0.3784012476539641i)
(nut = (make-polar 4 -1/8) '3.968790668917316-0.49869893354091077i)
(nut = (make-polar -1/8 4) '0.08170545260795149+0.09460031191349103i)
(nut = (make-polar -1 -4) '0.6536436208636119-0.7568024953079282i)
(nut = (make-polar -4 -1) '-2.161209223472559+3.365883939231586i)
(nut = (make-polar -1.0 1.0) '-0.5403023058681398-0.8414709848078965i)
(nut = (make-polar 1.0 -1.0) '0.5403023058681398-0.8414709848078965i)
(nut = (make-polar -1.0 4.0) '0.6536436208636119+0.7568024953079282i)
(nut = (make-polar 4.0 -1.0) '2.161209223472559-3.365883939231586i)
(nut = (make-polar -1.0 -1.0) '-0.5403023058681398+0.8414709848078965i)
(nut = (make-polar -1.0 -1.0) '-0.5403023058681398+0.8414709848078965i)
(nut = (make-polar -1.0 -4.0) '0.6536436208636119-0.7568024953079282i)
(nut = (make-polar -4.0 -1.0) '-2.161209223472559+3.365883939231586i)
(nut = (make-polar -1 1/2) '-0.8775825618903728-0.479425538604203i)
(nut = (make-polar 1/2 -1) '0.2701511529340699-0.42073549240394825i)
(nut = (make-polar -1 1/8) '-0.992197667229329-0.12467473338522769i)
(nut = (make-polar 1/8 -1) '0.06753778823351747-0.10518387310098706i)
(nut = (make-polar -1 -1/2) '-0.8775825618903728+0.479425538604203i)
(nut = (make-polar -1/2 -1) '-0.2701511529340699+0.42073549240394825i)
(nut = (make-polar -1 -1/8) '-0.992197667229329+0.12467473338522769i)
(nut = (make-polar -1/8 -1) '-0.06753778823351747+0.10518387310098706i)
(nut = (make-polar -4.0 1.0) '-2.161209223472559-3.365883939231586i)
(nut = (make-polar 1.0 -4.0) '-0.6536436208636119+0.7568024953079282i)
(nut = (make-polar -4.0 4.0) '2.6145744834544478+3.027209981231713i)
(nut = (make-polar 4.0 -4.0) '-2.6145744834544478+3.027209981231713i)
(nut = (make-polar -4.0 -1.0) '-2.161209223472559+3.365883939231586i)
(nut = (make-polar -1.0 -4.0) '0.6536436208636119-0.7568024953079282i)
(nut = (make-polar -4.0 -4.0) '2.6145744834544478-3.027209981231713i)
(nut = (make-polar -4.0 -4.0) '2.6145744834544478-3.027209981231713i)
(nut = (make-polar -4 1/2) '-3.510330247561491-1.917702154416812i)
(nut = (make-polar 1/2 -4) '-0.32682181043180597+0.3784012476539641i)
(nut = (make-polar -4 1/8) '-3.968790668917316-0.49869893354091077i)
(nut = (make-polar 1/8 -4) '-0.08170545260795149+0.09460031191349103i)
(nut = (make-polar -4 -1/2) '-3.510330247561491+1.917702154416812i)
(nut = (make-polar -1/2 -4) '0.32682181043180597-0.3784012476539641i)
(nut = (make-polar -4 -1/8) '-3.968790668917316+0.49869893354091077i)
(nut = (make-polar -1/8 -4) '0.08170545260795149-0.09460031191349103i)
(nut = (make-polar 1.0 4.0) '-0.6536436208636119-0.7568024953079282i)
(nut = (make-polar 4.0 1.0) '2.161209223472559+3.365883939231586i)
(nut = (make-polar 1.0 -1.0) '0.5403023058681398-0.8414709848078965i)
(nut = (make-polar -1.0 1.0) '-0.5403023058681398-0.8414709848078965i)
(nut = (make-polar 1.0 -4.0) '-0.6536436208636119+0.7568024953079282i)
(nut = (make-polar -4.0 1.0) '-2.161209223472559-3.365883939231586i)
(nut = (make-polar 1.0 0.5) '0.8775825618903728+0.479425538604203i)
(nut = (make-polar 0.5 1.0) '0.2701511529340699+0.42073549240394825i)
(nut = (make-polar 1.0 0.125) '0.992197667229329+0.12467473338522769i)
(nut = (make-polar 0.125 1.0) '0.06753778823351747+0.10518387310098706i)
(nut = (make-polar 1.0 -0.5) '0.8775825618903728-0.479425538604203i)
(nut = (make-polar -0.5 1.0) '-0.2701511529340699-0.42073549240394825i)
(nut = (make-polar 1.0 -0.125) '0.992197667229329-0.12467473338522769i)
(nut = (make-polar -0.125 1.0) '-0.06753778823351747-0.10518387310098706i)
(nut = (make-polar 4.0 -1.0) '2.161209223472559-3.365883939231586i)
(nut = (make-polar -1.0 4.0) '0.6536436208636119+0.7568024953079282i)
(nut = (make-polar 4.0 -4.0) '-2.6145744834544478+3.027209981231713i)
(nut = (make-polar -4.0 4.0) '2.6145744834544478+3.027209981231713i)
(nut = (make-polar 4.0 0.5) '3.510330247561491+1.917702154416812i)
(nut = (make-polar 0.5 4.0) '-0.32682181043180597-0.3784012476539641i)
(nut = (make-polar 4.0 0.125) '3.968790668917316+0.49869893354091077i)
(nut = (make-polar 0.125 4.0) '-0.08170545260795149-0.09460031191349103i)
(nut = (make-polar 4.0 -0.5) '3.510330247561491-1.917702154416812i)
(nut = (make-polar -0.5 4.0) '0.32682181043180597+0.3784012476539641i)
(nut = (make-polar 4.0 -0.125) '3.968790668917316-0.49869893354091077i)
(nut = (make-polar -0.125 4.0) '0.08170545260795149+0.09460031191349103i)
(nut = (make-polar -1.0 -4.0) '0.6536436208636119-0.7568024953079282i)
(nut = (make-polar -4.0 -1.0) '-2.161209223472559+3.365883939231586i)
(nut = (make-polar -1.0 0.5) '-0.8775825618903728-0.479425538604203i)
(nut = (make-polar 0.5 -1.0) '0.2701511529340699-0.42073549240394825i)
(nut = (make-polar -1.0 0.125) '-0.992197667229329-0.12467473338522769i)
(nut = (make-polar 0.125 -1.0) '0.06753778823351747-0.10518387310098706i)
(nut = (make-polar -1.0 -0.5) '-0.8775825618903728+0.479425538604203i)
(nut = (make-polar -0.5 -1.0) '-0.2701511529340699+0.42073549240394825i)
(nut = (make-polar -1.0 -0.125) '-0.992197667229329+0.12467473338522769i)
(nut = (make-polar -0.125 -1.0) '-0.06753778823351747+0.10518387310098706i)
(nut = (make-polar -4.0 0.5) '-3.510330247561491-1.917702154416812i)
(nut = (make-polar 0.5 -4.0) '-0.32682181043180597+0.3784012476539641i)
(nut = (make-polar -4.0 0.125) '-3.968790668917316-0.49869893354091077i)
(nut = (make-polar 0.125 -4.0) '-0.08170545260795149+0.09460031191349103i)
(nut = (make-polar -4.0 -0.5) '-3.510330247561491+1.917702154416812i)
(nut = (make-polar -0.5 -4.0) '0.32682181043180597-0.3784012476539641i)
(nut = (make-polar -4.0 -0.125) '-3.968790668917316+0.49869893354091077i)
(nut = (make-polar -0.125 -4.0) '0.08170545260795149-0.09460031191349103i)
(nut = (make-polar 1/2 1/8) '0.4960988336146645+0.062337366692613846i)
(nut = (make-polar 1/8 1/2) '0.1096978202362966+0.059928192325525376i)
(nut = (make-polar 1/2 -1/2) '0.4387912809451864-0.2397127693021015i)
(nut = (make-polar -1/2 1/2) '-0.4387912809451864-0.2397127693021015i)
(nut = (make-polar 1/2 -1/8) '0.4960988336146645-0.062337366692613846i)
(nut = (make-polar -1/8 1/2) '-0.1096978202362966-0.059928192325525376i)
(nut = (make-polar 1/8 -1/2) '0.1096978202362966-0.059928192325525376i)
(nut = (make-polar -1/2 1/8) '-0.4960988336146645-0.062337366692613846i)
(nut = (make-polar 1/8 -1/8) '0.12402470840366613-0.015584341673153462i)
(nut = (make-polar -1/8 1/8) '-0.12402470840366613-0.015584341673153462i)
(nut = (make-polar -1/2 -1/8) '-0.4960988336146645+0.062337366692613846i)
(nut = (make-polar -1/8 -1/2) '-0.1096978202362966+0.059928192325525376i)

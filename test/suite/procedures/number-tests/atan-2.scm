(load (path (path-parent #path 3) "lib.scm"))
(nut equal? (atan 0.0 0.0) '0.0)
(nut equal? (atan 0.0 0.0) '0.0)
(nut equal? (atan 0.0 Infinity) '0.0)
(nut equal? (atan Infinity 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 NaN) 'NaN)
(nut equal? (atan NaN 0.0) 'NaN)
(nut equal? (atan 0 1) '0)
(nut equal? (atan 1 0) '1.5707963267948966)
(nut equal? (atan 0 4) '0)
(nut equal? (atan 4 0) '1.5707963267948966)
(nut equal? (atan 0 -1) '3.141592653589793)
(nut equal? (atan -1 0) '-1.5707963267948966)
(nut equal? (atan 0 -4) '3.141592653589793)
(nut equal? (atan -4 0) '-1.5707963267948966)
(nut equal? (atan 0.0 1.0) '0.0)
(nut equal? (atan 1.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 4.0) '0.0)
(nut equal? (atan 4.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -1.0) '3.141592653589793)
(nut equal? (atan -1.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 -4.0) '3.141592653589793)
(nut equal? (atan -4.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0 1/2) '0)
(nut equal? (atan 1/2 0) '1.5707963267948966)
(nut equal? (atan 0 1/8) '0)
(nut equal? (atan 1/8 0) '1.5707963267948966)
(nut equal? (atan 0 -1/2) '3.141592653589793)
(nut equal? (atan -1/2 0) '-1.5707963267948966)
(nut equal? (atan 0 -1/8) '3.141592653589793)
(nut equal? (atan -1/8 0) '-1.5707963267948966)
(nut equal? (atan 0.0 Infinity) '0.0)
(nut equal? (atan Infinity 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 NaN) 'NaN)
(nut equal? (atan NaN 0.0) 'NaN)
(nut equal? (atan 0.0 1.0) '0.0)
(nut equal? (atan 1.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 4.0) '0.0)
(nut equal? (atan 4.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -1.0) '3.141592653589793)
(nut equal? (atan -1.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 -4.0) '3.141592653589793)
(nut equal? (atan -4.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 1.0) '0.0)
(nut equal? (atan 1.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 4.0) '0.0)
(nut equal? (atan 4.0 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -1.0) '3.141592653589793)
(nut equal? (atan -1.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 -4.0) '3.141592653589793)
(nut equal? (atan -4.0 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 0.5) '0.0)
(nut equal? (atan 0.5 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 0.125) '0.0)
(nut equal? (atan 0.125 0.0) '1.5707963267948966)
(nut equal? (atan 0.0 -0.5) '3.141592653589793)
(nut equal? (atan -0.5 0.0) '-1.5707963267948966)
(nut equal? (atan 0.0 -0.125) '3.141592653589793)
(nut equal? (atan -0.125 0.0) '-1.5707963267948966)
(nut equal? (atan Infinity -Infinity) '2.356194490192345)
(nut equal? (atan -Infinity Infinity) '-0.7853981633974483)
(nut equal? (atan Infinity NaN) 'NaN)
(nut equal? (atan NaN Infinity) 'NaN)
(nut equal? (atan Infinity 1.0) '1.5707963267948966)
(nut equal? (atan 1.0 Infinity) '0.0)
(nut equal? (atan Infinity 4.0) '1.5707963267948966)
(nut equal? (atan 4.0 Infinity) '0.0)
(nut equal? (atan Infinity -1.0) '1.5707963267948966)
(nut equal? (atan -1.0 Infinity) '-0.0)
(nut equal? (atan Infinity -4.0) '1.5707963267948966)
(nut equal? (atan -4.0 Infinity) '-0.0)
(nut equal? (atan Infinity 1.0) '1.5707963267948966)
(nut equal? (atan 1.0 Infinity) '0.0)
(nut equal? (atan Infinity 4.0) '1.5707963267948966)
(nut equal? (atan 4.0 Infinity) '0.0)
(nut equal? (atan Infinity -1.0) '1.5707963267948966)
(nut equal? (atan -1.0 Infinity) '-0.0)
(nut equal? (atan Infinity -4.0) '1.5707963267948966)
(nut equal? (atan -4.0 Infinity) '-0.0)
(nut equal? (atan Infinity 0.5) '1.5707963267948966)
(nut equal? (atan 0.5 Infinity) '0.0)
(nut equal? (atan Infinity 0.125) '1.5707963267948966)
(nut equal? (atan 0.125 Infinity) '0.0)
(nut equal? (atan Infinity -0.5) '1.5707963267948966)
(nut equal? (atan -0.5 Infinity) '-0.0)
(nut equal? (atan Infinity -0.125) '1.5707963267948966)
(nut equal? (atan -0.125 Infinity) '-0.0)
(nut equal? (atan -Infinity NaN) 'NaN)
(nut equal? (atan NaN -Infinity) 'NaN)
(nut equal? (atan -Infinity 1.0) '-1.5707963267948966)
(nut equal? (atan 1.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity 4.0) '-1.5707963267948966)
(nut equal? (atan 4.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity -1.0) '-1.5707963267948966)
(nut equal? (atan -1.0 -Infinity) '-3.141592653589793)
(nut equal? (atan -Infinity -4.0) '-1.5707963267948966)
(nut equal? (atan -4.0 -Infinity) '-3.141592653589793)
(nut equal? (atan -Infinity 1.0) '-1.5707963267948966)
(nut equal? (atan 1.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity 4.0) '-1.5707963267948966)
(nut equal? (atan 4.0 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity -1.0) '-1.5707963267948966)
(nut equal? (atan -1.0 -Infinity) '-3.141592653589793)
(nut equal? (atan -Infinity -4.0) '-1.5707963267948966)
(nut equal? (atan -4.0 -Infinity) '-3.141592653589793)
(nut equal? (atan -Infinity 0.5) '-1.5707963267948966)
(nut equal? (atan 0.5 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity 0.125) '-1.5707963267948966)
(nut equal? (atan 0.125 -Infinity) '3.141592653589793)
(nut equal? (atan -Infinity -0.5) '-1.5707963267948966)
(nut equal? (atan -0.5 -Infinity) '-3.141592653589793)
(nut equal? (atan -Infinity -0.125) '-1.5707963267948966)
(nut equal? (atan -0.125 -Infinity) '-3.141592653589793)
(nut equal? (atan NaN 1.0) 'NaN)
(nut equal? (atan 1.0 NaN) 'NaN)
(nut equal? (atan NaN 4.0) 'NaN)
(nut equal? (atan 4.0 NaN) 'NaN)
(nut equal? (atan NaN -1.0) 'NaN)
(nut equal? (atan -1.0 NaN) 'NaN)
(nut equal? (atan NaN -4.0) 'NaN)
(nut equal? (atan -4.0 NaN) 'NaN)
(nut equal? (atan NaN 1.0) 'NaN)
(nut equal? (atan 1.0 NaN) 'NaN)
(nut equal? (atan NaN 4.0) 'NaN)
(nut equal? (atan 4.0 NaN) 'NaN)
(nut equal? (atan NaN -1.0) 'NaN)
(nut equal? (atan -1.0 NaN) 'NaN)
(nut equal? (atan NaN -4.0) 'NaN)
(nut equal? (atan -4.0 NaN) 'NaN)
(nut equal? (atan NaN 0.5) 'NaN)
(nut equal? (atan 0.5 NaN) 'NaN)
(nut equal? (atan NaN 0.125) 'NaN)
(nut equal? (atan 0.125 NaN) 'NaN)
(nut equal? (atan NaN -0.5) 'NaN)
(nut equal? (atan -0.5 NaN) 'NaN)
(nut equal? (atan NaN -0.125) 'NaN)
(nut equal? (atan -0.125 NaN) 'NaN)
(nut equal? (atan 1 4) '0.24497866312686414)
(nut equal? (atan 4 1) '1.3258176636680326)
(nut equal? (atan 1 -1) '2.356194490192345)
(nut equal? (atan -1 1) '-0.7853981633974483)
(nut equal? (atan 1 -4) '2.896613990462929)
(nut equal? (atan -4 1) '-1.3258176636680326)
(nut equal? (atan 1.0 1.0) '0.7853981633974483)
(nut equal? (atan 1.0 1.0) '0.7853981633974483)
(nut equal? (atan 1.0 4.0) '0.24497866312686414)
(nut equal? (atan 4.0 1.0) '1.3258176636680326)
(nut equal? (atan 1.0 -1.0) '2.356194490192345)
(nut equal? (atan -1.0 1.0) '-0.7853981633974483)
(nut equal? (atan 1.0 -4.0) '2.896613990462929)
(nut equal? (atan -4.0 1.0) '-1.3258176636680326)
(nut equal? (atan 1 1/2) '1.1071487177940904)
(nut equal? (atan 1/2 1) '0.46364760900080615)
(nut equal? (atan 1 1/8) '1.446441332248135)
(nut equal? (atan 1/8 1) '0.12435499454676144)
(nut equal? (atan 1 -1/2) '2.0344439357957027)
(nut equal? (atan -1/2 1) '-0.46364760900080615)
(nut equal? (atan 1 -1/8) '1.695151321341658)
(nut equal? (atan -1/8 1) '-0.12435499454676144)
(nut equal? (atan 4 -1) '1.8157749899217608)
(nut equal? (atan -1 4) '-0.24497866312686414)
(nut equal? (atan 4 -4) '2.356194490192345)
(nut equal? (atan -4 4) '-0.7853981633974483)
(nut equal? (atan 4.0 1.0) '1.3258176636680326)
(nut equal? (atan 1.0 4.0) '0.24497866312686414)
(nut equal? (atan 4.0 4.0) '0.7853981633974483)
(nut equal? (atan 4.0 4.0) '0.7853981633974483)
(nut equal? (atan 4.0 -1.0) '1.8157749899217608)
(nut equal? (atan -1.0 4.0) '-0.24497866312686414)
(nut equal? (atan 4.0 -4.0) '2.356194490192345)
(nut equal? (atan -4.0 4.0) '-0.7853981633974483)
(nut equal? (atan 4 1/2) '1.446441332248135)
(nut equal? (atan 1/2 4) '0.12435499454676144)
(nut equal? (atan 4 1/8) '1.5395564933646282)
(nut equal? (atan 1/8 4) '0.031239833430268277)
(nut equal? (atan 4 -1/2) '1.695151321341658)
(nut equal? (atan -1/2 4) '-0.12435499454676144)
(nut equal? (atan 4 -1/8) '1.602036160225165)
(nut equal? (atan -1/8 4) '-0.031239833430268277)
(nut equal? (atan -1 -4) '-2.896613990462929)
(nut equal? (atan -4 -1) '-1.8157749899217608)
(nut equal? (atan -1.0 1.0) '-0.7853981633974483)
(nut equal? (atan 1.0 -1.0) '2.356194490192345)
(nut equal? (atan -1.0 4.0) '-0.24497866312686414)
(nut equal? (atan 4.0 -1.0) '1.8157749899217608)
(nut equal? (atan -1.0 -1.0) '-2.356194490192345)
(nut equal? (atan -1.0 -1.0) '-2.356194490192345)
(nut equal? (atan -1.0 -4.0) '-2.896613990462929)
(nut equal? (atan -4.0 -1.0) '-1.8157749899217608)
(nut equal? (atan -1 1/2) '-1.1071487177940904)
(nut equal? (atan 1/2 -1) '2.677945044588987)
(nut equal? (atan -1 1/8) '-1.446441332248135)
(nut equal? (atan 1/8 -1) '3.017237659043032)
(nut equal? (atan -1 -1/2) '-2.0344439357957027)
(nut equal? (atan -1/2 -1) '-2.677945044588987)
(nut equal? (atan -1 -1/8) '-1.695151321341658)
(nut equal? (atan -1/8 -1) '-3.017237659043032)
(nut equal? (atan -4.0 1.0) '-1.3258176636680326)
(nut equal? (atan 1.0 -4.0) '2.896613990462929)
(nut equal? (atan -4.0 4.0) '-0.7853981633974483)
(nut equal? (atan 4.0 -4.0) '2.356194490192345)
(nut equal? (atan -4.0 -1.0) '-1.8157749899217608)
(nut equal? (atan -1.0 -4.0) '-2.896613990462929)
(nut equal? (atan -4.0 -4.0) '-2.356194490192345)
(nut equal? (atan -4.0 -4.0) '-2.356194490192345)
(nut equal? (atan -4 1/2) '-1.446441332248135)
(nut equal? (atan 1/2 -4) '3.017237659043032)
(nut equal? (atan -4 1/8) '-1.5395564933646282)
(nut equal? (atan 1/8 -4) '3.110352820159525)
(nut equal? (atan -4 -1/2) '-1.695151321341658)
(nut equal? (atan -1/2 -4) '-3.017237659043032)
(nut equal? (atan -4 -1/8) '-1.602036160225165)
(nut equal? (atan -1/8 -4) '-3.110352820159525)
(nut equal? (atan 1.0 4.0) '0.24497866312686414)
(nut equal? (atan 4.0 1.0) '1.3258176636680326)
(nut equal? (atan 1.0 -1.0) '2.356194490192345)
(nut equal? (atan -1.0 1.0) '-0.7853981633974483)
(nut equal? (atan 1.0 -4.0) '2.896613990462929)
(nut equal? (atan -4.0 1.0) '-1.3258176636680326)
(nut equal? (atan 1.0 0.5) '1.1071487177940904)
(nut equal? (atan 0.5 1.0) '0.4636476090008061)
(nut equal? (atan 1.0 0.125) '1.446441332248135)
(nut equal? (atan 0.125 1.0) '0.12435499454676144)
(nut equal? (atan 1.0 -0.5) '2.0344439357957027)
(nut equal? (atan -0.5 1.0) '-0.4636476090008061)
(nut equal? (atan 1.0 -0.125) '1.695151321341658)
(nut equal? (atan -0.125 1.0) '-0.12435499454676144)
(nut equal? (atan 4.0 -1.0) '1.8157749899217608)
(nut equal? (atan -1.0 4.0) '-0.24497866312686414)
(nut equal? (atan 4.0 -4.0) '2.356194490192345)
(nut equal? (atan -4.0 4.0) '-0.7853981633974483)
(nut equal? (atan 4.0 0.5) '1.446441332248135)
(nut equal? (atan 0.5 4.0) '0.12435499454676144)
(nut equal? (atan 4.0 0.125) '1.5395564933646284)
(nut equal? (atan 0.125 4.0) '0.031239833430268277)
(nut equal? (atan 4.0 -0.5) '1.695151321341658)
(nut equal? (atan -0.5 4.0) '-0.12435499454676144)
(nut equal? (atan 4.0 -0.125) '1.602036160225165)
(nut equal? (atan -0.125 4.0) '-0.031239833430268277)
(nut equal? (atan -1.0 -4.0) '-2.896613990462929)
(nut equal? (atan -4.0 -1.0) '-1.8157749899217608)
(nut equal? (atan -1.0 0.5) '-1.1071487177940904)
(nut equal? (atan 0.5 -1.0) '2.677945044588987)
(nut equal? (atan -1.0 0.125) '-1.446441332248135)
(nut equal? (atan 0.125 -1.0) '3.017237659043032)
(nut equal? (atan -1.0 -0.5) '-2.0344439357957027)
(nut equal? (atan -0.5 -1.0) '-2.677945044588987)
(nut equal? (atan -1.0 -0.125) '-1.695151321341658)
(nut equal? (atan -0.125 -1.0) '-3.017237659043032)
(nut equal? (atan -4.0 0.5) '-1.446441332248135)
(nut equal? (atan 0.5 -4.0) '3.017237659043032)
(nut equal? (atan -4.0 0.125) '-1.5395564933646284)
(nut equal? (atan 0.125 -4.0) '3.110352820159525)
(nut equal? (atan -4.0 -0.5) '-1.695151321341658)
(nut equal? (atan -0.5 -4.0) '-3.017237659043032)
(nut equal? (atan -4.0 -0.125) '-1.602036160225165)
(nut equal? (atan -0.125 -4.0) '-3.110352820159525)
(nut equal? (atan 1/2 1/8) '1.3258176636680323)
(nut equal? (atan 1/8 1/2) '0.24497866312686414)
(nut equal? (atan 1/2 -1/2) '2.356194490192345)
(nut equal? (atan -1/2 1/2) '-0.7853981633974483)
(nut equal? (atan 1/2 -1/8) '1.8157749899217608)
(nut equal? (atan -1/8 1/2) '-0.24497866312686414)
(nut equal? (atan 1/8 -1/2) '2.896613990462929)
(nut equal? (atan -1/2 1/8) '-1.3258176636680323)
(nut equal? (atan 1/8 -1/8) '2.356194490192345)
(nut equal? (atan -1/8 1/8) '-0.7853981633974483)
(nut equal? (atan -1/2 -1/8) '-1.8157749899217608)
(nut equal? (atan -1/8 -1/2) '-2.896613990462929)
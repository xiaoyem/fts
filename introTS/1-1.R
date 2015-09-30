#1-1
require(fBasics)
da = read.table("d-axp3dx-0111.txt", header=T)
rtn = da[, 2:5]
#(a)
apply(rtn * 100, 2, basicStats)
#(b)
lrtn = log(1 + rtn)
apply(lrtn * 100, 2, basicStats)
#(c)
apply(lrtn, 2, t.test)
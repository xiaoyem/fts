#
# Copyright (c) 2015 by Yuchao Zhao, Xiaoye Meng.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
require(tsDyn)
require(urca)
require(vars)
da = read.table("data/m-gs1n3-5304.txt", header = TRUE)
head(da)
x1=da[,1]
x3=da[,2]
m1=VAR(cbind(x1,x3),p=3,ic=c("AIC"))
summary(m1)
var3.irf=irf(m1,period=6)
plot(var3.irf)
predict(m1,12)
cot=ca.jo(cbind(x1,x3),ecdet="const",type='trace',K=2,spec='transitory')
summary(cot)
vecm.fit=VECM(cbind(x1,x3),2)
summary(vecm.fit) 
vecm.fst=predict(vecm.fit,n.ahead = 12)
vecm.fst
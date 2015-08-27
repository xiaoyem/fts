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
source("hfrtn.R")
da=read.table("mmm9912-dtp2.txt",header=FALSE)
n1=hfrtn(da,5)
names(n1)
ts.plot(n1$rtn,main="5-m log returns")
Box.test(n1$rtn,lag=10,type='Ljung')
length(n1$rtn)/77
for (i in 31){
idx=(i-1)*77
tmp=sum(n1$rtn[(idx+1):(idx+77)]^2)
v1=c(v1,tmp)
}
v1=sqrt(v1)
v1
plot(v1,type='l')
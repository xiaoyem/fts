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
da1=read.table("taq-td-ba1.txt",header=TRUE)
da2=read.table("taq-td-ba2.txt",header=TRUE)
da3=read.table("taq-td-ba3.txt",header=TRUE)
da4=read.table("taq-td-ba4.txt",header=TRUE)
da5=read.table("taq-td-ba5.txt",header=TRUE)
total=rbind(da1,da2,da3,da4,da5)
n1=hfrtn(total,10)
ts.plot(n1$rtn,main="10-m log returns")
Box.test(n1$rtn,lag=10,type='Ljung')
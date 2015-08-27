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
source("acd.R")
da=read.table("mmm9912-adur.txt",header=FALSE)
head(da)
ad=da[,1]
acf(ad)
m1=acd(ad)
Box.test(m1$epsilon,     lag = 10, type = 'Ljung')
Box.test(m1$epsilon ^ 2, lag = 10, type = 'Ljung')
m2 = acd(ad, cond.dist = "weibull")
Box.test(m2$epsilon,     lag = 10, type = 'Ljung')
Box.test(m2$epsilon ^ 2, lag = 10, type = 'Ljung')
m3 = acd(ad, cond.dist = "gamma")
Box.test(m3$epsilon,     lag = 10, type = 'Ljung')
Box.test(m3$epsilon ^ 2, lag = 10, type = 'Ljung')

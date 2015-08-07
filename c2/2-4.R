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
require(fBasics)
require(fUnitRoots)
da = read.table("data/m-deciles08.txt", header = T)
d2  = da[, 3]
d10 = da[, 5]
Box.test(d2,  lag = 12, type = 'Ljung')
Box.test(d10, lag = 12, type = 'Ljung')
plot(ts(d2, start = c(1970, 1), frequency = 12), xlab = 'year', ylab = 'd2')
m0=ar(d2,method='mle')
d2=as.character(d2)
d2=as.numeric(d2)
adfTest(d2,lags=m0$order,type=c("c"))
acf(d2)
m1 = arima(d2, order = c(0, 0, 1))
m1
tsdiag(m1, gof = 12)
predict(m1, 12)

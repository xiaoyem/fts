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

require(xts)
require(fGarch)

da = read.table("data/m-mrk4608.txt", header = T)
mrk = log(1 + da[, 2])
plot(xts(mrk, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'mrk')
Box.test(mrk, lag = 12, type = 'Ljung')
par(mfrow = c(2, 1))
acf(mrk)
pacf(mrk)
m1 = arima(mrk, order = c(0, 0, 1))
m1
Box.test(m1$residuals,     lag = 10, type = 'Ljung')
Box.test(m1$residuals ^ 2, lag = 12, type = 'Ljung')
par(mfrow = c(1, 1))
pacf(m1$residuals ^ 2)
m2 = garchFit(~ arma(0, 1) + garch(3, 0), data = mrk, trace = F)
summary(m2)


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

da = read.table("data/d-gmsp9908.txt", header = T)
sp = da[, 3]
plot(xts(sp, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'sp')
Box.test(sp, lag = 10, type = 'Ljung')
par(mfrow = c(2, 1))
acf(sp)
pacf(sp)
m1 = arima(sp, order = c(2, 0, 0))
m1
Box.test(m1$residuals ^ 2, lag = 10, type = 'Ljung')
m2 = garchFit(~ arma(2, 0) + garch(1, 1), data = sp, trace = F)
summary(m2)
predict(m2, 4)


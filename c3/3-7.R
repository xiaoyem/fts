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
require(rugarch)

da = read.table("data/m-3m4608.txt", header = T)
mmm = log(1 + da[, 2])
plot(xts(mmm, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'mmm')
Box.test(mmm,     lag = 12, type = 'Ljung')
Box.test(mmm ^ 2, lag = 6,  type = 'Ljung')
Box.test(mmm ^ 2, lag = 12, type = 'Ljung')
pacf(mmm ^ 2)
m1 = garchFit(~ arma(0, 0) + garch(2, 0), data = mmm, trace = F)
summary(m1)
mmma = mmm[1:750]
m2 = garchFit(~ arma(0, 0) + garch(2, 0), data = mmma, trace = F)
summary(m2)
predict(m2, 5)
m3 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
	mean.model = list(armaOrder = c(0, 0), archm = T)), mmm)
show(m3)
m4 = ugarchfit(ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
	mean.model = list(armaOrder = c(0, 0))), mmma)
show(m4)
ugarchforecast(m4, n.ahead = 5)


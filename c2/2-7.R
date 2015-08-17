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
require(sandwich)
require(lmtest)

da = read.table("data/d-ibm3dxwkdays8008.txt", header = T)
ew = da$ew * 100
plot(xts(ew, order.by = as.Date(paste(da$year, da$mom, da$day, sep = '-'))),
	type = 'l', main = '', xlab = 'date', ylab = 'ew')
M = da$M
T = da$T
W = da$W
R = da$R
m1 = lm(ew ~ M + T + W + R)
summary(m1)
# FIXME
coeftest(m1, NeweyWest(m1, lag = 12, prewhite = F))
Box.test(m1$residuals, lag = 12, type = 'Ljung')
par(mfrow = c(2, 1))
acf(m1$residuals)
pacf(m1$residuals)
acf(diff(m1$residuals, 5))
pacf(diff(m1$residuals, 5))
# FIXME
m2 = arima(ew, order = c(2, 0, 2), seasonal = list(order = c(1, 0, 0), period = 5), xreg = da[, 8:11])
m2
tsdiag(m2, gof = 20)


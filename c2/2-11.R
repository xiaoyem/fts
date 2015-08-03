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

da = read.table("data/w-Aaa.txt", header = F)
Aaa = da[, 4]
plot(xts(Aaa, order.by = as.Date(paste(da[, 1], da[, 2], da[, 3], sep = '-'))),
	type = 'l', main = '', xlab = 'date', ylab = 'Aaa')
cat("order =", ar(diff(Aaa), method = 'mle')$order, "\n")
m1 = arima(Aaa, order = c(9, 1, 0))
m1
par(mfrow = c(2, 1))
acf(diff(Aaa))
pacf(diff(Aaa))
m2 = arima(Aaa, order = c(2, 1, 3))
m2
Box.test(m2$residuals, lag = 10, type = 'Ljung')


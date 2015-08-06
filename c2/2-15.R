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

da = read.table("data/q-gdpdef.txt", header = T)
y = log(da$gdpdef)
plot(xts(y, order.by = as.Date(paste(da$year, da$mom, da$day, sep = '-'))),
	type = 'o', main = '', xlab = 'date', ylab = 'y')
par(mfrow = c(2, 1))
acf(diff(y))
pacf(diff(y))
m1 = arima(diff(y), order = c(2, 0, 1))
m1
tsdiag(m1, gof = 20)
m2 = arima(diff(y), order = c(2, 0, 1), seasonal = list(order = c(0, 0, 1), period = 7))
m2
tsdiag(m2, gof = 20)
mp = predict(m2, 4)
Y1 = y[length(y)] + mp$pred[1]
Y2 = Y1 + mp$pred[2]
Y3 = Y2 + mp$pred[3]
Y4 = Y3 + mp$pred[4]
cat("prediction =", Y1, Y2, Y3, Y4, "\n");


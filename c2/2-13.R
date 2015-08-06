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

da = read.table("data/m-ew6299.txt", header = F)
lrtn = da[, 1]
plot(lrtn, type = 'l', ylab = 'lrtn')
t.test(lrtn)
par(mfrow = c(2, 1))
acf(lrtn)
pacf(lrtn)
m1 = arima(lrtn, order = c(1, 0, 0))
m1
Box.test(m1$residuals, lag = 12, type = 'Ljung')
m2 = arima(lrtn, order = c(0, 0, 1))
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')
predict(m1, 2)
predict(m2, 2)


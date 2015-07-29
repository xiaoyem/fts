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

da = read.table("data/m-unrate.txt", header = T)
rate = da[, 4]
plot(ts(rate, start = c(1948, 1), frequency = 12), type = 'o', xlab = 'year', ylab = 'rate')
acf(rate)
acf(diff(rate))
pacf(diff(rate))
cat("order =", ar(rate, method = 'mle')$order, "\n")
m1 = arima(rate, order = c(11, 0, 0))
m1
m1 = arima(rate, order = c(11, 0, 0), fixed = c(NA, NA, 0, 0, 0, NA, 0, 0, 0, NA, NA, NA))
m1
m2 = arima(rate, order = c(2, 1, 1), seasonal = list(order = c(1, 0, 1), period = 12), include.mean = F)
m2
tsdiag(m1, gof = 36)
tsdiag(m2, gof = 36)
predict(m1, 4)
predict(m2, 4)


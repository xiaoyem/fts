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

da = read.table("data/power6.txt", header = F)
pow = da[, 1]
plot(pow, type = 'o', ylab = 'pow')
par(mfrow = c(2, 1))
acf(pow)
pacf(pow)
acf(diff(pow))
pacf(diff(pow))
acf(diff(pow, 12))
pacf(diff(pow, 12))
acf(diff(diff(pow), 12))
pacf(diff(diff(pow), 12))
m1 = arima(pow, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
m1
tsdiag(m1, gof = 36)
predict(m1, 24)


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

da = read.table("data/w-Aaa.txt", header = F)
Aaa = da[, 4]
da = read.table("data/w-Baa.txt", header = F)
Baa = da[, 4]
m1 = lm(diff(Aaa) ~ diff(Baa))
summary(m1)
par(mfrow = c(2, 1))
acf(m1$residuals)
pacf(m1$residuals)
m2 = arima(diff(Aaa), order = c(4, 0, 0), xreg = diff(Baa), include.mean = F)
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')


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

da = read.table("data/d-ibm3dxwkdays8008.txt", header = T)
ibm = da$ibm * 100
plot(ibm, type = 'l')
M = da$M
T = da$T
W = da$W
R = da$R
F = da$F
m1 = lm(ibm ~ 0 + M + T + W + R + F)
summary(m1)
Box.test(m1$residuals, lag = 12, type = 'Ljung')
acf(m1$residuals)
acf(ibm)
pacf(ibm)
m2 = arima(ibm, order = c(0, 0, 1), xreg = da[, 8:12], include.mean = F)
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')


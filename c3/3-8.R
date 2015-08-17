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

da = read.table("data/m-gmsp5008.txt", header = T)
gm = log(1 + da[, 2]) * 100
plot(xts(gm, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'gm')
acf(gm)
m1 = garchFit(~ arma(0, 0) + garch(1, 1), data = gm, trace = F)
summary(m1)
predict(m1, 6)
m2 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
	mean.model = list(armaOrder = c(0, 0), archm = T)), gm)
show(m2)
ugarchforecast(m2, n.ahead = 6)
m3 = garchFit(~ arma(0, 0) + garch(1, 1), data = gm, cond.dist = "std", trace = F)
summary(m3)
predict(m3, 6)
# FIXME
cat("tt =", (9.41762 - 6) / 2.96756, "\n");
m4 = ugarchfit(ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
	mean.model = list(armaOrder = c(0, 0))), gm / 100)
show(m4)
ugarchforecast(m4, n.ahead = 6)


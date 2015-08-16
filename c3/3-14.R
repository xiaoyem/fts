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

da = read.table("data/d-gmsp9908.txt", header = T)
gm = log(1 + da[, 2]) * 100
sp = log(1 + da[, 3]) * 100
plot(xts(gm, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'gm')
plot(xts(sp, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'sp')
m1 = garchFit(~ arma(5, 0) + garch(1, 1), data = sp, cond.dist = "ged", trace = F)
summary(m1)
spvol = m1@sigma.t
m2 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
	external.regressors = as.matrix(spvol)), mean.model = list(armaOrder = c(0, 0)),
	distribution.model = "ged"), gm)
show(m2)


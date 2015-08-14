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
gm = log(1 + da[, 2])
sp = log(1 + da[, 3])
plot(xts(sp, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'sp')
m1 = garchFit(~ garch(1, 1), data = sp, trace = F)
summary(m1)
it = rep(c(0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0), 59)
m2 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
	external.regressors = as.matrix(it)), mean.model = list(armaOrder = c(0, 0))), sp)
show(m2)
gmsq = (gm - mean(gm)) ^ 2
m3 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
	external.regressors = as.matrix(gmsq[1:707])), mean.model = list(armaOrder = c(0, 0))), sp[2:708])
show(m3)


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

da = read.table("data/d-gmsp9908.txt", header = T)
gm = log(1 + da[, 2])
plot(xts(gm, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'gm')
Box.test(gm ^ 2, lag = 10, type = 'Ljung')
pacf(gm ^ 2, 10)
m1 = garchFit(~ garch(1, 1), data = gm, trace = F)
summary(m1)
m2 = garchFit(~ garch(1, 1), data = gm, cond.dist = "ged", trace = F)
summary(m2)


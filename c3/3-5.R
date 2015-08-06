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

da = read.table("data/m-intc7308.txt", header = T)
lrtn = log(1 + da[, 2])
plot(xts(lrtn, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'lrtn')
m1 = garchFit(~ garch(1, 1), data = lrtn, trace = F)
summary(m1)
m2 = garchFit(~ garch(1, 1), data = lrtn, cond.dist = 'std', trace = F)
summary(m2)
Box.test(m2@residuals, lag = 10, type = 'Ljung')
predict(m2, 5)


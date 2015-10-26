#
# Copyright (c) 2015 by Gaohang Wu.
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

#7-2
#(a)
da = read.table("d-aapl-0111.txt", header = T)
lrtn = log(da[,2] + 1)
tdx = c(1:length(lrtn)) / 270 + 2001
plot(tdx, lrtn, xlab = "year", ylab = "ln-rtn", type = "l")
prob = c(0.9, 0.95, 0.99, 0.999)
quantile(lrtn, prob)
slrtn = sort(lrtn)
#0.95*2704=2568.8 0.99*2704=2676.96
es_05 = sum(slrtn[2570:2704]) / (2704-2569)
es_01 = sum(slrtn[2678:2704]) / (2704-2677)

#(b)
require(fGarch)
source("RMeasure.R")
m1 = garchFit(~ arma(0, 0) + garch(1, 1), data = lrtn, trace = F)
summary(m1)
require(quantreg)
m2 = rq(lrtn~volatility(m1), tau = 0.95, data = da)
summary(m2)
m3 = rq(lrtn~volatility(m1), tau = 0.99, data = da)
summary(m3)

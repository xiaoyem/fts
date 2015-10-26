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

#7-1
#(a)
da = read.table("d-aapl-0111.txt", header = T)
lrtn = log(da[,2] + 1)
tdx = c(1:length(lrtn)) / 270 + 2001
plot(tdx, lrtn, xlab = "year", ylab = "ln-rtn", type = "l")
source("RMfit.R")
mm = RMfit(lrtn)

#(b)
require(fGarch)
source("RMeasure.R")
m1 = garchFit(~ arma(0, 0) + garch(1, 1), data = lrtn, trace = F)
summary(m1)
predict(m1, 10)
#mean=0.00219 vol=0.01927
m11 = RMeasure(0.00219, 0.01927)

#(c)
m2 = garchFit(~ arma(0, 0) + garch(1, 1), data = lrtn, cond.dist = "std", trace = F)
summary(m2)
predict(m2, 10)
#mean:0.00179 vol=0.01920
m21 = RMeasure(0.00179, 0.01920)
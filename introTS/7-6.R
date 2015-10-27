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

#7-6
#(a)
da = read.table("d-aaplbnd-0111.txt", header = T)
lappl = da[,2]
lbnd = da[,3]
par(mfcol = c(2,1))
tdx = c(1:length(lappl)) / 270 + 2001
plot(tdx, lappl, xlab = "year", ylab = "ln-apple", type = "l")
plot(tdx, lbnd, xlab = "year", ylab = "ln-bnd", type = "l")
source("RMfit.R")
mm1 = RMfit(lappl)
mm2 = RMfit(lbnd)

#(b)
require(fGarch)
source("RMeasure.R")
m1 = garchFit(~ arma(0, 0) + garch(1, 1), data = lappl, trace = F)
summary(m1)
predict(m1, 3)
m2 = garchFit(~ arma(0, 0) + garch(1, 1), data = lbnd, trace = F)
summary(m2)
predict(m2, 3)
#mean=0.00219 vol=0.01927
m11 = RMeasure(0.002191419, 0.01926286)
m21 = RMeasure(0.0002274955, 0.004505358)
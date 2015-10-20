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

#5-4
require(fBasics)
da = read.table("d-a2a-0110.txt", header = T)
alc = da[,1]
abt = da[,3]
axp = da[,9]
par(mfrow = c(2, 1))
require(fGarch)
rtn = cbind(alc, abt, axp)
v1 = cov(rtn[1946:2515,])
v1inv = solve(v1)#solve: inverse matrix
one = matrix(1, 3, 1)#matrix(row,col,byrow)
wgt = v1inv%*%one#%*%: algebraic product
d = sum(wgt*one)
wgt = wgt / d
print(wgt)

source("GMVP.R")
m1 = GMVP(rtn, start = 1946)#FIX IT
names(m1)
weight = m1$weights
range(weight)
prtn = m1$returns

mean(prtn)
sqrt(var(prtn))

cov1 = sqrt(apply(rtn[1946:2515,], 2, var))
minVar = sqrt(m1$minVariance)
vol = sqrt(m1$variances)
range(minVar, vol)

tdx=c(1:570) / 252 + 2008.83
plot(tdx, weight[1,], xlab = "year", ylab = "weights", type = "l")
lines(tdx, weight[2,], lty = 2)
lines(tdx, weight[3,], lty = 3)

plot(tdx, vol[,1], xlab = "year", ylab = "vol", type = "l")
lines(tdx, vol[,2], lty = 2)
lines(tdx, vol[,3], lty = 3)
lines(tdx, minVar, lty = 4)
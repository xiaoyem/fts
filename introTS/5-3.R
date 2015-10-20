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

#5-3
require(fBasics)
da = read.table("d-abtsp-0110.txt", header = T)
abt = da[,2]
sp5 = da[,3]
require(fGarch)
xp = abt + sp5
xm = abt - sp5
m1 = garchFit(~1 + garch(1,1),data = xp, trace = F)
summary(m1)
m2 = garchFit(~1 + garch(1,1),data = xm, trace = F)
summary(m2)
m3 = garchFit(~1 + garch(1,1),data = sp5, trace = F)
summary(m3)
vxp = volatility(m1)
vxm = volatility(m2)
vsp5 = volatility(m3)
#beta P196 P200
beta = (vxp^2 - vxm^2) / (4*vsp5^2)
idx = c(1:2515)[beta==max(beta)]
idx
beta[idx]#find max beta, beta>1: high risk

tdx = c(1:2515) / (252) + 2001
m4 = lm(abt ~ sp5)
summary(m4)
plot(tdx, beta, xlab = "year", ylab = "beta", type = "l")
abline(h = 1)#add one or more straight line in current plot


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

require(fGarch)

da = read.table("data/d-hpq3dx9808.txt", header = T)
hpq = log(1 + da[, 2])
sp  = log(1 + da[, 5])
# FIXME
source("c7/RMfit.R")
RMfit(hpq)
RMfit(sp)
cor(hpq, sp)
sqrt(0.08279960 ^ 2 + 0.07410689 ^ 2 + 2 * 0.5606587 * 0.08279960 * 0.07410689)
m1 = garchFit(~ arma(0, 0) + garch(1, 1), data = hpq, trace = F)
summary(m1)
m1p = predict(m1, 1)
source("c7/RMeasure.R")
RMeasure(-m1p$meanForecast[1], m1p$meanError[1])
m2 = garchFit(~ arma(0, 0) + garch(1, 1), data = sp, trace = F)
summary(m2)
m2p = predict(m2, 1)
RMeasure(-m2p$meanForecast[1], m2p$meanError[1])
sqrt(0.08531052 ^ 2 + 0.06171560 ^ 2 + 2 * 0.5606587 * 0.08531052 * 0.06171560)


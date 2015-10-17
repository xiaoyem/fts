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
require(evir)

da = read.table("data/d-ge9808.txt", header = T)
ge = log(1 + da[, 2])
# FIXME
source("c7/RMfit.R")
RMfit(ge)
sqrt(15) * 0.1157275
m1 = garchFit(~ arma(2, 0) + garch(1, 1), data = ge, trace = F)
summary(m1)
m2 = garchFit(~ arma(0, 0) + garch(1, 1), data = ge, trace = F)
summary(m2)
m2p = predict(m2, 15)
source("c7/RMeasure.R")
RMeasure(-m2p$meanForecast[1], m2p$meanError[1])
RMeasure(-sum(m2p$meanForecast), sqrt(sum(m2p$meanError ^ 2)))
m3 = garchFit(~ arma(0, 0) + garch(1, 1), data = ge, cond.dist = "std", trace = F)
summary(m3)
m3p = predict(m3, 15)
RMeasure(-m3p$meanForecast[1], m3p$meanError[1], "std", 7.6335)
RMeasure(-sum(m3p$meanForecast), sqrt(sum(m3p$meanError ^ 2)), "std", 7.6335)
m4 = gev(-ge, 21)
m4
source("c7/evtVaR.R")
evtVaR(0.32662479, 0.01241653, 0.02126877)
15 ^ 0.32662479 * 0.04643982


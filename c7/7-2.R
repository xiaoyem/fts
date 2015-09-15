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
da = read.table("data/d-csco9808.txt", header = TRUE)
head(da)
rtn=log(1 + da[, 2])
source("RMfit.R")
RMfit(rtn)
m1=garchFit(~garch(1,1),data=rtn,trace=FALSE)
summary(m1)
m1p=predict(m1,1)
m1p
pre1=0.001283774-2.326348*0.03091923
pre1
m2=garchFit(~arma(0,0)+garch(1,1),data=rtn,cond.dist = "std",trace=FALSE)
summary(m2)
m2p=predict(m2,1)
m2p
pre1=0.0007615308-2.326348*0.03953755
pre1
quantile(rtn,0.01)
m3=gpd(-rtn,threshold = 0.02)
summary(m3)
par(mfcol=c(2,2))
plot(m3)
riskmeasures(m3,c(0.99))
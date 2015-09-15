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
da = read.table("data/d-ge9808.txt", header = TRUE)
head(da)
dim(da)
ge=da[,2]
source("RMfit.R")
RMfit(ge)
m2=garchFit(~arma(1,0)+garch(1,1),data=ge,trace=FALSE)
summary(m2)
m3=garchFit(~arma(0,0)+garch(1,1),data=ge,trace=FALSE)
summary(m3)
m3p=predict(m3,15)
m3p
pre1=0.000393-2.326348*0.04962843
abs(pre1)*1000000
pre15=sum(m3p$meanForecast)-2.326348*sqrt(sum(m3p$meanError^2))
abs(pre15)*1000000
m4=garchFit(~arma(0,0)+garch(1,1),data=ge,cond.dist = "std",trace=FALSE)
summary(m4)
m4p=predict(m3,15)
m4p
pre1=0.000236-2.326348*0.04929286
abs(pre1)*1000000
pre15=sum(m4p$meanForecast)-2.326348*sqrt(sum(m4p$meanError^2))
abs(pre15)*1000000
nge=-ge
m5=gev(nge,block = 21)
m5
source("evtVaR.R")
evtVaR(0.3084,0.012140,0.021065,n=21,p=0.01)
0.04530087*1000000

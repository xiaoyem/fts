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
da = read.table("data/d-hpq3dx9808.txt", header = TRUE)
head(da)
lhp=log(da[,2]+1)
lsp=log(da[,5]+1)
cor(lhp,lsp)
source("RMfit.R")
RMfit(lhp)
RMfit(lsp)
source("Igarch.R")
m1=Igarch(lhp)
m2=Igarch(lsp)
VaR=sqrt(0.0827996^2+0.07410689^2+2*cor(lhp,lsp)*0.0827996*0.07410689)
VaR*1000000
m3=garchFit(~garch(1,1),data=lhp,trace=FALSE)
summary(m3)
m3p=predict(m3,1)
m3p
m4=garchFit(~garch(1,1),data=lsp,trace=FALSE)
m4p=predict(m4,1)
m4p
var1=0.0007396582-2.326348*0.03698938
var2=0.0002592816-2.326348*0.02664042
VaR=sqrt(var1^2+var2^2+2*cor(lsp,lhp)*var1*var2)
VaR*1000000
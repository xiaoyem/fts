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
da=read.table("data/m-excess-c10sp-9003.txt",header = TRUE)
head(da)
dim(da)
drug=c(rep(1,4),rep(0,6))
auto=c(rep(0,4),rep(1,2),rep(0,4))
oil=c(rep(0,6),rep(1,4))
beta=cbind(drug,auto,oil)
beta
rtn=da[,1:10]
rtn=t(rtn)
F.hat.o=solve(crossprod(beta))%*%t(beta)%*%rtn
E.hat.o=rtn-beta%*%F.hat.o
diagD.hat=apply(E.hat.o,1,var)
Dinv.hat=diag(diagD.hat^(-1))
Hmtx=solve(t(beta)%*%Dinv.hat%*%beta)%*%t(beta)%*%Dinv.hat
F.hat=Hmtx%*%rtn
F.hat=t(F.hat)
t(Hmtx)
cov.model=beta%*%var(F.hat)%*%t(beta)+diag(diagD.hat)
sd.model=sqrt(diag(cov.model))
corr.model=cov.model/outer(sd.model,sd.model)
print(corr.model,digits = 1,width=2)
dim(F.hat)
par(mfcol=c(3,1))
plot(1:168,F.hat[,1],type='l',main = 'Factor realization: Big drug companies')
plot(1:168,F.hat[,2],type='l',main = 'Auto industry')
plot(1:168,F.hat[,3],type='l',main = 'Oil companies')
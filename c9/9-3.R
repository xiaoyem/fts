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
da=read.table("data/m-excess-c10sp-9003.txt",header=TRUE)
head(da)
dim(da)
rtn=da[,1:10]
rtn=as.matrix(rtn)
xmtx=cbind(rep(1,168),da[,11])
xit.hat=qr.solve(xmtx,rtn)
beta.hat=xit.hat[2,]
E.hat=rtn-xmtx%*%xit.hat
D.hat=diag(crossprod(as.matrix(E.hat))/(168-2))
r.squre=1-(168-2)*D.hat/diag(t(as.matrix(rtn))%*%as.matrix(rtn))
t(rbind(beta.hat,sqrt(D.hat),r.squre))
opt=t(rbind(beta.hat,sqrt(D.hat),r.squre))
par(mfcol=c(2,1))
barplot(opt[,1],main = 'Beta-hat')
barplot(opt[,3],main = 'R-squre')
cov.model=var(da[,11])*(beta.hat%*%t(beta.hat))+diag(D.hat)
sd.model=sqrt(diag(cov.model))
corr.model=cov.model/outer(sd.model,sd.model)
corr.model
cor(rtn)
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

require(nnet)

da = read.table("data/m-ge2608.txt", header = T)
dim(da)
y = da[4:960, 2]
xx = cbind(da[3:959, 2], da[2:958, 2], da[1:957, 2])
m1 = nnet(xx, y, size = 2, linout = T, skip = T, maxit = 3000)
summary(m1)
yp = da[961:996, 2]
xp = cbind(da[960:995, 2], da[959:994, 2], da[958:993, 2])
mse = sum((yp - predict(m1, xp)) ^ 2) / 36
cat("mse =", mse, "\n");

yd=ifelse(y>0,1,0)
x1=da[3:959, 2]
x2=da[2:958, 2]
x3=da[1:957, 2]
x1d=ifelse(x1>0,1,0)
x2d=ifelse(x2>0,1,0)
x3d=ifelse(x3>0,1,0)
xx=cbind(x1,x2,x3,x1d,x2d,x3d)
m2=nnet(xx,yd,size=5,linout=T,skip=T,maxit=3000)
ydhat=ifelse(m2$fitted.values>0.5,1,0)
yp=da[961:996, 2]
x1p=da[960:995, 2]
x2p=da[959:994, 2]
x3p=da[958:993, 2]
x1pd=ifelse(x1p>0,1,0)
x2pd=ifelse(x2p>0,1,0)
x3pd=ifelse(x3p>0,1,0)
xp=cbind(x1p,x2p,x3p,x1pd,x2pd,x3pd)
ypredict=predict(m2,xp)
ypredict2=ifelse(ypredict>0.5,1,0)
ypredict2
mse = sum((yp - predict(m2, xp)) ^ 2) / 36
mse
ypd=ifelse(yp>0,1,0)
mse = sum((ypd - ypredict2) ^ 2) / 36
mse

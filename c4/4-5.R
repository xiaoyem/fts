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
y = da[4:960, 2]
xx = cbind(da[3:959, 2], da[2:958, 2], da[1:957, 2])
m1 = nnet(xx, y, size = 2, linout = T, skip = T, maxit = 3000)
summary(m1)
yp = da[961:996, 2]
xp = cbind(da[960:995, 2], da[959:994, 2], da[958:993, 2])
mse = sum((yp - predict(m1, xp)) ^ 2) / 36
cat("mse =", mse, "\n")
# FIXME
yd = ifelse(da[4:960, 2] > 0, 1, 0)
xx = cbind(xx, ifelse(da[3:959, 2] > 0, 1, 0), ifelse(da[2:958, 2] > 0, 1, 0),
	ifelse(da[1:957, 2] > 0, 1, 0))
m2 = nnet(xx, yd, size = 5, skip = T, maxit = 3000)
summary(m2)
ypd = ifelse(da[961:996, 2] > 0, 1, 0)
xp = cbind(xp, ifelse(da[960:995, 2] > 0, 1, 0), ifelse(da[959:994, 2] > 0, 1, 0),
	ifelse(da[958:993, 2] > 0, 1, 0))
mse = sum((ypd - ifelse(predict(m1, xp) > 0.5, 1, 0)) ^ 2) / 36
cat("mse =", mse, "\n")


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

require(MTS)

da = read.table("data/m-fedip.txt", header = T)
y = cbind(diff(da[, 3]), diff(da[, 4]))
VARorder(y)
m1 = VAR(y, 2)
res = m1$residuals[424:591, 1:2]
da = read.table("data/m-excess-c10sp-9003.txt", header = T)
xmtx = cbind(rep(1, 168), res)
rtn = as.matrix(da[, 1:10])
xit.hat = solve(t(xmtx) %*% xmtx) %*% (t(xmtx) %*% rtn)
beta.hat = t(xit.hat[2:3, ])
E.hat = rtn - xmtx %*% xit.hat
D.hat = diag(t(E.hat) %*% E.hat / (168 - 3))
r.square = 1 - (168 - 3) * D.hat / diag(t(rtn) %*% rtn)
cov.model = beta.hat %*% var(res) %*% t(beta.hat) + diag(D.hat)
sd.model = sqrt(diag(cov.model))
corr.model = cov.model / outer(sd.model, sd.model)
corr.model


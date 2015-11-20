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

da = read.table("data/m-excess-c10sp-9003.txt", header = T)
dim(da)
xmtx = cbind(rep(1, 168), da[, 11])
rtn = as.matrix(da[, 1:10])
xit.hat = solve(t(xmtx) %*% xmtx) %*% (t(xmtx) %*% rtn)
beta.hat = t(xit.hat[2, ])
E.hat = rtn - xmtx %*% xit.hat
D.hat = diag(t(E.hat) %*% E.hat / (168 - 2))
r.square = 1 - (168 - 2) * D.hat / diag(t(rtn) %*% rtn)
t(rbind(beta.hat, sqrt(D.hat), r.square))
par(mfcol = c(1, 2))
barplot(beta.hat, horiz = T, main = 'Beta values')
barplot(r.square, horiz = T, main = 'R-square')
cov.r = var(da[, 11]) * (t(beta.hat) %*% beta.hat) + diag(D.hat)
w.gmin.model = solve(cov.r) %*% rep(1, nrow(cov.r))
w.gmin.model = w.gmin.model / sum(w.gmin.model)
t(w.gmin.model)
w.gmin.data = solve(var(rtn)) %*% rep(1, nrow(cov.r))
w.gmin.data = w.gmin.data / sum(w.gmin.data)
t(w.gmin.data)


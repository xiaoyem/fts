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
drug = c(rep(1, 4), rep(0, 6))
auto = c(rep(0, 4), 1, 1, rep(0, 4))
oil  = c(rep(0, 6), rep(1, 4))
beta = cbind(drug, auto, oil)
rtn = t(da[, 1:10])
F.hat.o = solve(t(beta) %*% beta) %*% t(beta) %*% rtn
E.hat.o = rtn - beta %*% F.hat.o
diagD.hat.o = apply(E.hat.o, 1, var)
Dinv.hat = diag(diagD.hat.o ^ (-1))
Hmtx = solve(t(beta) %*% Dinv.hat %*% beta) %*% t(beta) %*% Dinv.hat
F.hat.g = Hmtx %*% rtn
F.hat.gt = t(F.hat.g)
E.hat.g = rtn - beta %*% F.hat.g
diagD.hat.g = apply(E.hat.g, 1, var)
t(Hmtx)
cov.model = beta %*% var(F.hat.gt) %*% t(beta) + diag(diagD.hat.g)
sd.model = sqrt(diag(cov.model))
corr.model = cov.model / outer(sd.model, sd.model)
corr.model
par(mfcol = c(3, 1))
plot(F.hat.gt[, 1], type = 'l', main = '(a) Factor realization: Big drug companies')
plot(F.hat.gt[, 2], type = 'l', main = '(b) Auto industry')
plot(F.hat.gt[, 3], type = 'l', main = '(c) Oil companies')


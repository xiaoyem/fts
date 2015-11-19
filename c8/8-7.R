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
require(urca)

da = read.table("data/m-gs1n3-5304.txt", header = F)
y = da[, 1:2]
# FIXME
VARorder(y, 12)
m1 = VAR(y, 3)
m1.irf = VARMAirf(m1$Phi, Sigma = m1$Sigma, lag = 6)
m1.irf$irf
VARpred(m1, 12)
cot = ca.jo(y, "trace", "const", 2, "transitory")
summary(cot)
m2 = ECMvar(y, 3, c(1.0000000, -1.0006509))


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

require(evir)

da = read.table("data/d-aaspx9808.txt", header = T)
aa = log(1 + da[, 2]) * 100
# FIXME
m1 = gev(aa, 21)
m1
#par(mfcol = c(1, 2))
#plot(m1)
par(mfcol = c(1, 1))
rl.21.24 = rlevel.gev(m1, 24)
rl.21.24
par(mfcol = c(1, 2))
qplot(aa, threshold = 0.025)
meplot(aa)
m2 = gpd(aa, 0.03)
m2
#par(mfcol = c(2, 2))
#plot(m2)
riskmeasures(m2, c(0.99, 0.999))


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

da = read.table("data/m-gs1n10.txt", header = T)
attach(da)
y = cbind(log(gs1), log(gs10))
# FIXME
VARorder(y, 12)
m1 = VARMA(y, 1, 9)
m1 = refVARMA(m1)
MTSdiag(m1)


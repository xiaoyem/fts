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

da = read.table("data/m-geibmsp2608.txt", header = T)
ge  = log(1 + da[, 2]) * 100
ibm = log(1 + da[, 3]) * 100
sp  = log(1 + da[, 4]) * 100
rtn = cbind(ge, ibm, sp)
p1 = dccPre(rtn)
m1 = dccFit(p1$sresi)


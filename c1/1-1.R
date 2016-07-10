#
# Copyright (c) 2015-2016 by Yuchao Zhao, Xiaoye Meng.
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

require(fBasics)

da = read.table("data/d-3stocks9908.txt", header = T)
rtn = da[, 2:4]
# (a)
apply(rtn * 100, 2, basicStats)
# (b)
lrtn = log(1 + rtn)
# (c)
apply(lrtn * 100, 2, basicStats)
# (d)
# \frac{\sqrt{T}\hat{\mu}_x}{\hat{\sigma}_x}
apply(lrtn, 2, t.test)


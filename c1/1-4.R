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

require(fBasics)

da = read.table("data/d-3stocks9908.txt", header = T)
laxp = log(1 + da[, 2])
t1 = skewness(laxp) / sqrt(6 / length(laxp))
cat("t1 =", t1, "\n")
p1 = pnorm(t1) * 2
cat("p1 =", p1, "\n")
t2 = kurtosis(laxp) / sqrt(24 / length(laxp))
cat("t2 =", t2, "\n")
p2 = (1 - pnorm(t2)) * 2
cat("p2 =", p2, "\n")


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

da = read.table("data/w-Aaa.txt", header = F)
Aaa = da[, 4]
da = read.table("data/w-Baa.txt", header = F)
Baa = da[, 4]
apply(cbind(Aaa, Baa), 2, basicStats)
ts = skewness(Aaa) / sqrt(6 / length(Aaa))
cat("ts =", ts, "\n")
ps = (1 - pnorm(ts)) * 2
cat("ps =", ps, "\n")
tk = kurtosis(Aaa) / sqrt(24 / length(Aaa))
cat("tk =", tk, "\n")
pk = (1 - pnorm(tk)) * 2
cat("pk =", pk, "\n")
ts = skewness(Baa) / sqrt(6 / length(Baa))
cat("ts =", ts, "\n")
ps = (1 - pnorm(ts)) * 2
cat("ps =", ps, "\n")
tk = kurtosis(Baa) / sqrt(24 / length(Baa))
cat("tk =", tk, "\n")
pk = (1 - pnorm(tk)) * 2
cat("pk =", pk, "\n")


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

da = read.table("data/mmm9912-dtp.txt", header = F)
T = dim(da)[1]
# FIXME
icnt = 0
prev = 0
while (icnt < T) {
	idx = c(1:T)[da[, 1] == da[icnt + 1, 1]]
	for (i in (icnt + 1):(icnt + length(idx))) {
		if (da[i, 1] > prev + 1) da[i, 1] = prev + 1
	}
	icnt = icnt + length(idx)
	prev = da[icnt, 1]
}
source("c5/hfntra.R")
hfntra(da, 5)
acf(diff(log(da[, 3])), plot = F)
pchg = NULL
for (i in 2:T) {
	if (da[i, 1] == da[i - 1, 1]) pchg = c(pchg, da[i, 3] - da[i - 1, 3])
}
tab = c(sum(pchg <= -5 / 16),
	sum(pchg <= -4 / 16 & pchg > -5 / 16),
	sum(pchg <= -3 / 16 & pchg > -4 / 16),
	sum(pchg <= -2 / 16 & pchg > -3 / 16),
	sum(pchg <= -1 / 16 & pchg > -2 / 16),
	sum(pchg <   1 / 16 & pchg > -1 / 16),
	sum(pchg <   2 / 16 & pchg >= 1 / 16),
	sum(pchg <   3 / 16 & pchg >= 2 / 16),
	sum(pchg <   4 / 16 & pchg >= 3 / 16),
	sum(pchg <   5 / 16 & pchg >= 4 / 16),
	sum(                  pchg >= 5 / 16))
names(tab) = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5")
tab


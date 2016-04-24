#
# Copyright (c) 2015-2016 by Gaohang Wu.
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

da_uk = read.table("introTS/d-fx-ukus-0711.txt", header = T)
da_jp = read.table("introTS/d-fx-usjp-0711.txt", header = T)
# (a)
diff_uk = diff(log(da_uk$rate))
diff_jp = diff(log(da_jp$rate))
# (b)
basicStats(diff_uk)
basicStats(diff_jp)
# (c)
d = density(diff_jp)
plot(d$x, d$y, type = 'l', xlab = 'log-rtn', ylab = 'density')
# (d)
# \frac{\sqrt{T}\hat{\mu}_x}{\hat{\sigma}_x}
res = t.test(diff_jp)
p = res$p.value
if (p > 0.05) {
	cat("p-value =", p, "> 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p, "<= 0.05, reject the null hypothesis\n")
}


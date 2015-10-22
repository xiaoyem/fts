#
# Copyright (c) 2015 by Gaohang Wu.
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

da = read.table("introTS/m-ge3dx-4011.txt", header = T)
T = length(da[, 2])
#(a)
res = t.test(da[, 2])
p = res$p.value
if (p > 0.05) {
	cat("p-value =", p, "> 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p, "<= 0.05, reject the null hypothesis\n")
}
#(b)
s1 = skewness(da[, 2])
t1 = s1 / sqrt(6 / T)
p1 = 2 * (1 - pnorm(t1))
if (p1 > 0.05) {
	cat("p-value =", p1, "> 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p1, "<= 0.05, reject the null hypothesis\n")
}
#(c)
s2 = kurtosis(da[, 2])
t2 = s2 / sqrt(24 / T)
p2 = 2 * (1 - pnorm(t2))
if (p2 > 0.05) {
	cat("p-value =", p2, "> 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p2, "<= 0.05, reject the null hypothesis\n")
}


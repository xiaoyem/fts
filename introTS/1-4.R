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

da = read.table("introTS/d-axp3dx-0111.txt", header = T)
T = length(da[, 2])
# (a)
# \frac{\hat{S}(r)}{\sqrt{6/T}}
s3 = skewness(log(1 + da[, 2]))
t3 = s3 / sqrt(6 / T)
p3 = 2 * (1 - pnorm(t3))
if (p3 > 0.05) {
	cat("p-value =", p3, " > 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p3, " <= 0.05, reject the null hypothesis\n")
}
# (b)
# \frac{\hat{K}(r) - 3}{\sqrt{24/T}}
s4 = kurtosis(log(1 + da[, 2]))
t4 = s4 / sqrt(24 / T)
p4 = 2 * (1 - pnorm(t4))
if (p4 > 0.05) {
	cat("p-value =", p4, " > 0.05, cannot reject the null hypothesis\n")
} else {
	cat("p-value =", p4, " <= 0.05, reject the null hypothesis\n")
}


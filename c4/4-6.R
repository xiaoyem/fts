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

require(TSA)

da1 = read.table("data/w-gs1yr.txt", header = T)
da2 = read.table("data/w-gs3yr.txt", header = T)
s1 = da2$rate - da1$rate
s2 = diff(da2$rate) - diff(da1$rate)
cat("order1 =", ar(s1, method = 'mle')$order, "\n")
cat("order2 =", ar(s2, method = 'mle')$order, "\n")
Tsay.test(s1, 11)
Tsay.test(s2, 12)
Keenan.test(s1, 11)
Keenan.test(s2, 12)
tlrt(s1, 11)
tlrt(s2, 12)
m1 = tar(s1, 11, 11, 1)
m1$qr1$coefficients
m1$qr2$coefficients
m2 = tar(s2, 12, 12, 1)
m2$qr1$coefficients
m2$qr2$coefficients


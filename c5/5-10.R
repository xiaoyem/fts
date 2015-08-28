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

da1 = read.table("data/taq-td-ba12012008.txt", header = T)
da2 = read.table("data/taq-td-ba12022008.txt", header = T)
da3 = read.table("data/taq-td-ba12032008.txt", header = T)
da4 = read.table("data/taq-td-ba12042008.txt", header = T)
da5 = read.table("data/taq-td-ba12052008.txt", header = T)
c1 = diff(da1[, 4])
c2 = diff(da2[, 4])
c3 = diff(da3[, 4])
c4 = diff(da4[, 4])
c5 = diff(da5[, 4])
s = sum(c1 == 0) + sum(c2 == 0) + sum(c3 == 0) + sum(c4 == 0) + sum(c5 == 0)
t = length(c1)   + length(c2)   + length(c3)   + length(c4)   + length(c5)
cat(s, "/", t, "=", s / t, "\n")


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

da = read.table("data/m-mrk2vw.txt", header = T)
m1 = princomp(da[, 2:7])
summary(m1)
m1$loadings
m2 = princomp(da[, 2:7], cor = T)
summary(m2)
m2$loadings
# FIXME
m3 = factanal(da[, 2:7], 2)
m3$loadings


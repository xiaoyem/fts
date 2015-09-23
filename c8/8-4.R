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
require(vars)
require(urca)
da = read.table("data/m-gs1n10.txt", header = TRUE)
attach(da)
cot=ca.jo(cbind(gs1,gs10),ecdet="const",type='trace',K=2,spec='transitory')
summary(cot)
m1=VAR(cbind(gs1,gs10),p=9,ic=c("AIC"))
summary(m1)
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
date = rep(1, nrow(da1))
da1 = data.frame(date, da1)
da2 = read.table("data/taq-td-ba12022008.txt", header = T)
date = rep(2, nrow(da2))
da2 = data.frame(date, da2)
da3 = read.table("data/taq-td-ba12032008.txt", header = T)
date = rep(3, nrow(da3))
da3 = data.frame(date, da3)
da4 = read.table("data/taq-td-ba12042008.txt", header = T)
date = rep(4, nrow(da4))
da4 = data.frame(date, da4)
da5 = read.table("data/taq-td-ba12052008.txt", header = T)
date = rep(5, nrow(da5))
da5 = data.frame(date, da5)
source("c5/hfrtn.R")
hfrtn = hfrtn(rbind(da1, da2, da3, da4, da5), 10, collapsed = F, averaged = F)
par(mfrow = c(2, 1))
plot(hfrtn$rtn,   type = 'l', main = '10-minute intraday returns', ylab = 'returns')
plot(hfrtn$price, type = 'l', main = 'price',                      ylab = 'price')
Box.test(hfrtn$rtn, lag = 10, type = 'Ljung')


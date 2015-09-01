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
require(fGarch)
da = read.table("data/mmm9912-dtp.txt", header = F)
T = dim(da)[1]
# FIXME
icnt = 0;
prev = 0;
while (icnt < T) {
	idx = c(1:T)[da[, 1] == da[icnt + 1, 1]]
	for (i in (icnt + 1):(icnt + length(idx))) {
		if (da[i, 1] > prev + 1) da[i, 1] = prev + 1
	}
	icnt = icnt + length(idx)
	prev = da[icnt, 1]
}
source("c5/hfrtn.R")
hfrtn = hfrtn(da, 5)
par(mfrow = c(2, 1))
plot(hfrtn$rtn,   type = 'l', main = '5-minute intraday returns', ylab = 'returns');

plot(hfrtn$price, type = 'l', main = 'price',                     ylab = 'price');
Box.test(hfrtn$rtn, lag = 10, type = 'Ljung')
# FIXME
vol = NULL
for (i in 0:21) {
	vol = c(vol, sum(hfrtn$rtn[(77 * i + 1):(77 * i + 77)] ^ 2))
}
plot(vol, type = 'l', main = 'Under independence', xlab = 'day')
m1=garchFit(hfrtn$rtn~arma(1,0)+garch(1,1),data=hfrtn$rtn)
vol=m1@sigma.t
plot(vol,type='l')
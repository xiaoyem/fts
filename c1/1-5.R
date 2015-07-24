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

require(fBasics)

da = read.table("data/d-caus.txt", header = T)
caus = diff(log(da$rate))
basicStats(caus)
da = read.table("data/d-useu.txt", header = T)
useu = diff(log(da$Value))
basicStats(useu)
da = read.table("data/d-jpus.txt", header = T)
jpus = diff(log(da$value))
basicStats(jpus)
da = read.table("data/d-usuk.txt", header = T)
usuk = diff(log(da$value))
basicStats(usuk)
d = density(useu)
par(mfcol = c(1, 1))
plot(d$x, d$y, xlab = 'log-rtn', ylab = 'density', type = 'l')


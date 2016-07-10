#
# Copyright (c) 2015-2016 by Yuchao Zhao, Xiaoye Meng.
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
db = read.table("data/d-useu.txt", header = T)
dc = read.table("data/d-jpus.txt", header = T)
dd = read.table("data/d-usuk.txt", header = T)
# (a)
caus = diff(log(da$rate))
useu = diff(log(db$Value))
jpus = diff(log(dc$value))
usuk = diff(log(dd$value))
# (b)
basicStats(caus)
basicStats(useu)
basicStats(jpus)
basicStats(usuk)
# (d)
d = density(useu)
plot(d$x, d$y, type = 'l', xlab = 'log-rtn', ylab = 'density')


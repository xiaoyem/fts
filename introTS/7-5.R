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

#7-5
library(evir)
da = read.table("d-aapl-0111.txt", header = T)
lrtn = log(da[,2] + 1)
xt = -lrtn
m1gpd = gpd(xt, threshold = 0.025)
m1gpd
par(mfcol = c(2,2))
plot(m1gpd)
riskmeasures(m1gpd, 0.99)
var10_01_21 = 10^(-0.03379731) * 0.06983398
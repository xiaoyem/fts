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

#7-3
#(a)
library(evir)
da = read.table("d-aapl-0111.txt", header = T)
lrtn = log(da[,2] + 1)

xt=-lrtn
source("Hill.R")
par(mfcol=c(2,1))
m1 = gev(xt,block = 21)
m2 = gev(xt,block = 42)

#(b)
source("evtVar.R")
#n = 21
#xi=0.13547661, sigma=0.01719197, mu=0.03379731,n,prob
m1
var1_05_21 = evtVaR(0.13547661, 0.01719197, 0.03379731,21,0.05)
var10_05_21 = 10^(-0.03379731) * 0.03252589
var1_01_21 = evtVaR(0.13547661, 0.01719197, 0.03379731,21,0.01)
var10_01_21 = 10^(-0.03379731) * 0.06356866

#n=42
m2
#xi=0.34890590, sigma=0.01595711, mu=0.04067976,n,prob
var1_05_42 = evtVaR(0.34890590, 0.01595711, 0.04067976,21,0.05)
var10_05_42 = 10^(-0.04067976) * 0.03950896
var1_01_42 = evtVaR(0.34890590, 0.01595711, 0.04067976,21,0.01)
var10_01_42 = 10^(-0.04067976) * 0.07364361
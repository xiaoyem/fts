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
#

#6-4
require(fBasics)
da = read.table("taq-aa-t-june72010.txt", header = T)
price = da[,5]

#(a)
price_diff = diff(price)
sec = 3600 * da$hour + 60 * da$minute + da$second
#trading hours: 9:30-16:00
ist = 3600*9 + 30*60 #market open
end = 3600*16 #market close
idx = c(1:length(sec))[sec < ist]
jdx = c(1:length(sec))[sec > end]
sec = sec[-c(idx,jdx)] #normal trading hours
price_diff = price_diff[sec]
plot(price_diff, type = "l")

#(b)
#price change rate
pch = c(0, 0, 0, 0, 0, 0, 0)
#category of price change
cpch = c(1:length(price_diff))
for (i in 1:length(price_diff))
{
  cpch[i] = 0
  if (price_diff[i] < -2)
  {
    cpch[i] = 1
    pch[1] = pch[1] + 1
  }
  if ((price_diff[i] >= -2) && (price_diff[i] < -1))
  {
    cpch[i] = 2
    pch[2] = pch[2] + 1
  }
  if ((price_diff[i] >= -1) && (price_diff[i] < -0))
  {
    cpch[i] = 3
    pch[3] = pch[3] + 1
  }
  if (price_diff[i] == 0)
  {
    cpch[i] = 4
    pch[4] = pch[4] + 1
  }
  if ((price_diff[i] > 0) && (price_diff[i] <= 1))
  {
    cpch[i] = 5
    pch[5] = pch[5] + 1
  }
  if ((price_diff[i] > 1) && (price_diff[i] <= 2))
  {
    cpch[i] = 6
    pch[6] = pch[6] + 1
  }
  if (price_diff[i] > 2)
  {
    cpch[i] = 7
    pch[7] = pch[7] + 1
  }
}
pch = pch / length(price_diff)

#(c)
#P227
vol = da$SIZE / 100
vol = vol[sec]
cf = as.factor(cpch)
length(cf) #78413
length(vol)
y = cf[4:78413]
y1 = cf[3:78412]
y2 = cf[2:78411]
vol = da$SIZE / 100
vol = vol[sec]
length(vol)
vol = vol[2:78412]
v2 = vol[2:78411]
cp1 = price_diff[3:78412]
cp2 = price_diff[2:78411]
cp3 = price_diff[1:78410]
m1 = polr(y~v2+cp1+cp2+cp3+y1+y2, method = "probit")
names(m1)
yhat = m1$fitted.values
print(yhat[1:5,], digits=3)
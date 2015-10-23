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
require(MASS)
vol = da$SIZE / 100
vol = vol[sec]
cf = as.factor(cpch)
length(cf) #78413
length(vol)
y = cf[4:78413]
y1 = cf[3:78412]
y2 = cf[2:78411]

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

#(d)
source("GeoSize.R")
idx = c(1:78413)[price_diff > 0]#[price_diff > 0]
jdx = c(1:78413)[price_diff < 0]#[price_diff < 0]
A = rep(0,78413)
A[idx] = 1
A[jdx] = 1
D = rep(0,78413)
D[idx] = 1
D[jdx] = -1
S = abs(cpch-4)
Ai = A[2:78413] #Ai = 1: price changed in the i_th trade
Aim1 = A[1:78412]
Di = D[2:78413] #Di = 1: price rose in the i_th trade
Dim1 = D[1:78412]
Si = S[2:78413] #Si: pch
Sim1 = S[1:78412]

m2 = glm(Ai~Aim1, family = "binomial")
summary(m2)

di = Di[Ai == 1]
dim1 = Dim1[Ai == 1]
di = (di+abs(di)) / 2
m3 = glm(di~dim1, family = "binomial")
summary(m3)

#si = Si[Di == 1]
#sim1 = Sim1[(Di == 1)]
#m4 = GeoSize(si, sim1)
##Error in solve.default(Hessian) : 
##system is computationally singular: reciprocal condition number = 0
#summary(m4)

#nsi = Si[Di == -1]
#nsim1 = Sim1[Di == -1]
#m5 = GeoSize(nsi, nsim1)
#Error in solve.default(Hessian) : 
#system is computationally singular: reciprocal condition number = 0
#summary(m5)
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

#5-5
require(fBasics)
da = read.table("d-a2a-0110.txt", header = T)
aapl = da[,2]
appl = log(appl + 1)

require(fGarch)
m1 = garchFit(~1 + garch(1,1),data = aapl, trace = F)
summary(m1)
sigma = volatility(m1)

#Option pricing P195
r = 0.01 #risk-free rate
K = 355 #option exercise price
N = 10 #repeat N times
Ti = 10 #time

#get final price P[]
P = c(1:10)
for (i in 1:N)
{
  epsilon = rnorm(10)
  p0 = 350 #current price
  for (t in 1:Ti)
  {
    #P195 (5-4)
    p0 = p0 * exp(r - 0.5*mean(sigma)^2 + mean(sigma)*epsilon[t])
  }
  P[i] = p0 #final price
}

temp_e = 0
temp_a = 0
for (i in 1:N)
{
  temp_e = temp_e + max(P[i] - K, 0)
  temp_a = temp_a + max(mean(P) - K, 0)
}
e = exp(1) #e = 2.71828
euro = e^(-r*Ti) * temp_e / N #European call option
asia = e^(-r*Ti) * temp_a / N #Asian call option
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

# 6-7
#(a)
da=read.table("taq-sbux-jul2011.txt",header=T)
da25 = da[da$date == "20110725",]
da26 = da[da$date == "20110726",]
da27 = da[da$date == "20110727",]
da28 = da[da$date == "20110728",]
da29 = da[da$date == "20110729",]

# get normal trading hours data
NTHD <- function(da)
{
  sec = 3600 * da$hour + 60 * da$min + da$sec
  #trading hours: 9:30-16:00
  ist = 3600*9 + 30*60 #market open
  end = 3600*16 #market close
  idx = c(1:length(sec))[sec < ist]
  jdx = c(1:length(sec))[sec > end]
  da = da[(idx[length(idx)]+1):(jdx[1]-1),]
  return (da)
}

da25 = NTHD(da25)
da26 = NTHD(da26)
da27= NTHD(da27)
da28 = NTHD(da28)
da29 = NTHD(da29)

par(mfrow = c(2, 1))
source("hfrtn.R")
data = rbind(da25, da26, da27, da28, da29)
hfrtn = hfrtn(data, 5, collapsed = F, averaged = F)
tdx = c(1:length(hfrtn$rtn)) / (6.5*12) + 25
plot(tdx, hfrtn$rtn, xlab = "trade", ylab = "ln-rtn", type = "l")

freq = rep(0,25) # -0.010~0.015
for (i in 1:length(hfrtn$rtn))
{
  #change rtn to index
  index = (hfrtn$rtn[i] %/% 0.001) + 10
  freq[index] = freq[index] + 1
}
barplot(freq, ylab = "frequency", xlab = "Rolation")

#(b)
#P246
source("hfanal.R")
m1 = hfanal(data, 5)
names(m1)
Ytot = m1$Ytot
Rv = cbind(Ytot, m1$realized)

#(c)and(d)
#P249
source("hf2ts.R")
m2 = hf2ts(data, int = 5)
names(m2)
#average
mRV = m2$ave.RV
#2-scale method
RV = m2$realized
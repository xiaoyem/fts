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

#6-6
require(fBasics)
da_all = read.table("taq-aa-t-june7t112010.txt", header = T)
da07 = da_all[da_all$Date == "20100607",]
da08 = da_all[da_all$Date == "20100608",]
da09 = da_all[da_all$Date == "20100609",]
da10 = da_all[da_all$Date == "20100610",]
da11 = da_all[da_all$Date == "20100611",]

# get normal trading hours data
NTHD <- function(da)
{
  sec = 3600 * da$hour + 60 * da$minute + da$second
  #trading hours: 9:30-16:00
  ist = 3600*9 + 30*60 #market open
  end = 3600*16 #market close
  idx = c(1:length(sec))[sec < ist]
  jdx = c(1:length(sec))[sec > end]
  da = da[(idx[length(idx)]+1):(jdx[1]-1),]
  return (da)
}

da07 = NTHD(da07)
da08 = NTHD(da08)
da09 = NTHD(da09)
da10 = NTHD(da10)
da11 = NTHD(da11)

#xt
#change hour_minute_second to t
HMS_to_t <- function(hour, minute)
{
  hour = hour - 9 #start: 9:00
  result = hour * 12 + minute %/% 5 + 1
  result = result - 6 #start: 9:30
  return (result)
}
#6.5*12*5=390
x = rep(0,390)

#too slow, fix it
for (i in 1:length(da07[,1]))
{
  t = HMS_to_t(da07[i,]$hour, da07[i,]$minute)
  x[t] = x[t] + 1
}
for (i in 1:length(da08[,1]))
{
  t = HMS_to_t(da08[i,]$hour, da08[i,]$minute)
  t = t + 6.5 * 12
  x[t] = x[t] + 1
}
for (i in 1:length(da09[,1]))
{
  t = HMS_to_t(da09[i,]$hour, da09[i,]$minute)
  t = t + 6.5 * 12 * 2
  x[t] = x[t] + 1
}
for (i in 1:length(da10[,1]))
{
  t = HMS_to_t(da10[i,]$hour, da10[i,]$minute)
  t = t + 6.5 * 12 * 3
  x[t] = x[t] + 1
}
for (i in 1:length(da11[,1]))
{
  t = HMS_to_t(da11[i,]$hour, da11[i,]$minute)
  t = t + 6.5 * 12 * 4
  x[t] = x[t] + 1
}

#diurnal pattern
#P249 hf2ts.R
plot(x, ylab = "trade", xlab = "time", type = "l")
for (i in 1:5)
{
  abline(v = 6.5 * 12 * i) 
}

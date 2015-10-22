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

#6-3
require(fBasics)
da = read.table("taq-aa-t-june72010.txt", header = T)
par(mfrow = c(2, 1))
sec = 3600 * da$hour + 60 * da$minute + da$second
#trading hours: 9:30-16:00
ist = 3600*9 + 30*60 #market open
end = 3600*16 #market close
lunch = 3600*12
#(a)
idx = c(1:length(sec))[sec < ist]
jdx = c(1:length(sec))[sec > end]
sec = sec[-c(idx,jdx)] #normal trading hours
dt = diff(sec)
kdx = c(1:length(dt))[dt > 0]
length(kdx) #none-zero conditional duration
dt = dt[kdx]
plot(dt, type = "l", xlab = "index", ylab = "duration")
mean(dt)
var(dt)

#(b)
ti = sec[2:length(sec)]
ti = ti[kdx]
st = 3600*6.5
f1 = (ti - lunch) / st
ft = cbind(f1, f1^2, log(ti))
m1 = lm(log(dt)~ft)
summary(m1)
fit = m1$fitted.values
adjdt = dt / exp(fit)
plot(adjdt, type = "l", xlab = "index", ylab = "adj-duration")

#(c)
source("acd.R")
m2 = acd(adjdt, order = c(1, 1), cond.dist = "exp")
Box.test(m2$epsilon,     lag = 10, type = 'Ljung')
Box.test(m2$epsilon ^ 2, lag = 10, type = 'Ljung')

#(d)
m3 = acd(adjdt, order = c(1, 1), cond.dist = "weibull")
Box.test(m3$epsilon,     lag = 10, type = 'Ljung')
Box.test(m3$epsilon ^ 2, lag = 10, type = 'Ljung')
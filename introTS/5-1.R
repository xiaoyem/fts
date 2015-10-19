#5-1
require(fUnitRoots)
da = read.table("w-petroprice.txt", header = T)
world = da[,4]
us = da[,5]
par(mfrow = c(2, 1))

#(a)
pacf(world)
p = 2
#q
acf(world)
q = 0
d = 0
m1 = arima(world, order = c(p, d, q))
m1
Box.test(m1$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(world)
auto_mode = arima(world, order = c(2,1,2))
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')

#(b)
#seasonal adjustment
world_ts =  ts(world,frequency=52,start=c(1997,1,3))
world_components = decompose(world_ts, f = 52)
world_ts_adj = world_ts - world_components$seasonal
tdx = c(1: 717) / 52 + 1997
plot(tdx, world_ts_adj,type='l',xlab='day',ylab='world_ts_adj')

#(c)
tdx = c(1: 717) / 52 + 1997
plot(tdx, world,type='l',xlab='day',ylab='world')
m2 = arima(world, order = c(p, d, q), seasonal = list(order = c(0,1,1), period = 13))
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')

#(d)
#Fix ARMA-GARCH model
require(fGarch)
m3 = garchFit(~arma(0,2) + garch(1,1),data = world, trace = F)
summary(m3)

#(e)
source("backtest.R")
mm1 = backtest(m1, world, 717-56, 1)
mm2 = backtest(m2, world, 717-56, 1)
#Fix ARMA-GARCH model
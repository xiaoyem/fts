#3-2
require(fUnitRoots)
da = read.table("m-morgfed-7111.txt", header = F)
morg = da[,4]
#(a)
d = 0
#p
pacf(morg)
p = 3
#q
acf(morg)
q = 0

m3 = arima(morg, order = c(p,d,q))
m3
Box.test(m3$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(morg)
auto_mode = arima(morg, order = c(2,1,3))
auto_mode
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')

#(b)
fed = da[,5]
m4 = lm(morg ~ -1 + fed)

summary(m4)

d = 0
pacf(m4$residuals)
acf(m4$residuals)
m4 
p = 2
q = 0
m5 = arima(morg, order = c(p,d,q), xreg = fed, include.mean = F)
m5
Box.test(m5$residuals, lag = 12, type = 'Ljung')

#(c)
source("backtest.R")
mm1 = backtest(m3, morg, 400, 1)
mm2 = backtest(m5, morg, 400, 1, xre = fed)
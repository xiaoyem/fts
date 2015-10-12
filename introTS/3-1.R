#3-1
require(fUnitRoots)
da = read.table("m-CAUS-7611.txt", header = T)
ca = da[,3]
#(a)
d = 0

acf(ca)
acf(diff(ca))

acf(diff(diff(ca)))
pacf(diff(diff(ca)))
p = 6
q = 3

m2 = arima(ca, order = c(p,d,q))
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
ca = diff(diff(ca))
auto.arima(ca)
auto_mode = arima(ca, order = c(1,0,0))
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')


#(b)
us = da[,4]
us = diff(diff(us))
m3 = lm(ca ~ -1 + us)

summary(m3)
#d
d = 0
#p q
pacf(m3$residuals)
acf(m3$residuals)
q = 2
p = 6
m6 = arima(ca, order = c(p,d,q), xreg = us, include.mean = F)
m6
Box.test(m6$residuals, lag = 12, type = 'Ljung')


#(c)
source("backtest.R")
mm1 = backtest(m2, ca, 385, 1)
mm2 = backtest(m6, ca, 385, 1, xre = us)
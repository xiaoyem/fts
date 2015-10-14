#4-5
da = read.table("d-pg-0111.txt", header = T)
rtn = da[,2]
rtn = log(rtn + 1)
#(a)
par(mfrow = c(2,1))
acf(rtn)
pacf(rtn)

#(b)
d = 0
p = 2
q = 0
m1 = arima(rtn, order = c(p,d,q))
m1

#(c)
x = m1$residuals * 100
Box.test(x, lag = 10, type = 'Ljung')

#(d)
#EGARCH P168-P172
source('Egarch.R')
m2 = Egarch(x)
stresi = m2$residuals / m2$volatility
#tdx = c(1:2535) / 365 + 2001
Box.test(stresi, lag = 10, type = 'Ljung')
Box.test(stresi, lag = 20, type = 'Ljung')
Box.test(stresi^2, lag = 10, type = 'Ljung')
Box.test(stresi^2, lag = 20, type = 'Ljung')
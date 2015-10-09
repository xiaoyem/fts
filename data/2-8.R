#2-8
da = read.table("q-GNPC96.txt", header = T)
gnp = da[,4]
gnp = diff(gnp)
#(a)
m1 = arima(gnp, order = c(4, 0, 0))
Box.test(m1$residuals, lag = 12, type = 'Ljung')
#(b)
m2 = arima(gnp, order = c(3, 0, 0))
Box.test(m2$residuals, lag = 12, type = 'Ljung')
#(c)
m1
m2
#(d)
source("backtest.R")
mm1 = backtest(m1, gnp, 233, 1)
mm2 = backtest(m2, gnp, 233, 1)
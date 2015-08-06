require(fBasics)
require(timeSeries)

da=read.table("m-ew6299.txt",header=T)
V1=da[,1]
acf(V1)
pacf(V1)
mean(V1)

m1=arima(V1,order=c(1,0,0))
m1
Box.test(m1$residuals,lag=12,type='Ljung')
m2=arima(V1,order=c(0,0,1))
m2
Box.test(m2$residuals,lag=12,type='Ljung')
predict(m1,2)
predict(m2,2)

da=read.table("sp5may.dat",head=T)

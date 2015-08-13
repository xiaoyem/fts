require(xts)
require(fGarch)
<<<<<<< HEAD
da = read.table("m-intc7308.txt", header = T)
head(da)
rtn=as.numeric(as.character(da[2:433,2]))
intc=log(rtn+1)
at=intc-mean(intc)
Box.test(intc,lag=12,type='Ljung')
Box.test(at^2,lag=12,type='Ljung')
acf(at)
acf(at^2)
acf(abs(at))
pacf(at^2)
=======

da = read.table("data/m-intc7308.txt", header = T)
intc = log(1 + da[, 2])
plot(xts(intc, order.by = as.Date(paste(substr(da[, 1], 1, 4), substr(da[, 1], 5, 6), substring(da[, 1], 7),
	sep = '-'))), type = 'l', main = '', xlab = 'date', ylab = 'intc')
>>>>>>> origin/master
m1 = garchFit(~ garch(1, 1), data = intc, trace = F)
summary(m1)
m2 = garchFit(~ garch(1, 1), data = intc, cond.dist = 'std', trace = F)
summary(m2)
Box.test(m2@residuals, lag = 10, type = 'Ljung')
predict(m2, 5)

#4-3
da = read.table("m-ko-6111.txt", header = T)
ko = da[,2]
ko = log(ko + 1)
#(a)
res = t.test(ko)
if (res$p.value > 0.05)
{
  cat("p-value = ",res$p.value," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",res$p.value," <= 0.05, reject the null hypothesis")
}

acf(ko, lag = 10)

y = ko - mean(ko)
Box.test(y^2, lag = 10, type = 'Ljung')

#(b)
pacf(y^2)

require(fGarch)
m1 = garchFit(~1 + garch(4,0), data = ko, trace = F)
summary(m1)

#(c)
m2 = garchFit(~1 + garch(4,0), data = ko, trace = F, cond.dist = "std")
summary(m2)
plot(m2)
13
predict(m2, 5)
resi = residuals(m2, standardize = T)
tdx = c(1: 609) / 12 + 1961
plot(tdx, resi, xlab = 'year', ylab = 'stand-resi', type = 'l')
acf(resi, lag = 10)

#4-1
require(fUnitRoots)
da = read.table("d-spy-0111.txt", header = T)
rtn = da[,2]
rtn = log(rtn + 1)
#rtn_ts=ts(rtn,frequency=365,start=c(2001,9,4))
#plot(rtn_ts,type='l',xlab='day',ylab='ln-rtn')

#(a)
res = t.test(rtn)
if (res$p.value > 0.05)
{
  cat("p-value = ",res$p.value," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",res$p.value," <= 0.05, reject the null hypothesis")
}

acf(rtn, lag = 10)
# ARCH effect P141-P142
y = rtn - mean(rtn)
Box.test(y^2, lag = 10, type = 'Ljung')

#(b)
#P150-P151
require(fGarch)
m1 = garchFit(~1 + garch(2,1), data = rtn, trace = F)
summary(m1)
plot(m1)
13
resi = residuals(m1, standardize = T)
tdx = c(1: 2535) / 365 + 2001
plot(tdx, resi, xlab = 'year', ylab = 'stand-resi', type = 'l')
acf(resi, lag = 10)

#(c)
#student t P153
m2 = garchFit(~1 + garch(2,1), data = rtn, trace = F, cond.dist = "std")
summary(m2)
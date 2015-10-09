#2-2
require(fUnitRoots)
da = read.table("m-dec125910-6111.txt", header = T)
d2 = da[,3]
d10 = da[,6]
#(a)
res = Box.test(d2,  lag = 12, type = 'Ljung')
pvalue = res$p.value
if (pvalue > 0.05)
{
  cat("p-value = ",pvalue," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",pvalue," <= 0.05, reject the null hypothesis")
}
res = Box.test(d10, lag = 12, type = 'Ljung')
pvalue = res$p.value
if (pvalue > 0.05)
{
  d = 1
  cat("p-value = ",pvalue," > 0.05, cannot reject the null hypothesis")
}else
{
  d = 0
  cat("p-value = ",pvalue," <= 0.05, reject the null hypothesis")
}
#(b)
#p
#m1 = ar(diff(d2), method='mle')
pacf(d2)
p = 0
#q
acf(d2)
q = 1
#d
temp = d2
while (pvalue > 0.05)
{
  d2 = diff(d2)
  m2 = ar(diff(d2), method = 'mle')
  res = adfTest(d2,lags=m2$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
m3 = arima(d2, order = c(p, d, q))
m3
#(c)
tsdiag(m3, gof = 12)
predict(m3, 12)
Box.test(m3$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(d2)
auto_mode = arima(d2, order = c(0,0,1))
predict(auto_mode, 4)
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')
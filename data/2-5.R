#2-5
da = read.table("m-aaa-1911.txt", header = T)
rate = da[,4]
lrate = log(rate)
#d
m1 = ar(diff(lrate), method='mle')
res = adfTest(lrate,lags=m1$order,type=c("c"))
pvalue = res@test$p.value
if (pvalue > 0.05)
{
  d = 1
}else
{
  d = 0
}
temp_rate = lrate
while (pvalue > 0.05)
{
  temp_rate = diff(temp_rate)
  m2 = ar(diff(temp_rate), method = 'mle')
  res = adfTest(temp_rate,lags=m2$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
#p
pacf(diff(lrate))
p = 2
#q
acf(diff(lrate))
q = 1
m3 = arima(lrate, order = c(p, d, q), seasonal = list(order = c(0, 1, 1), period = 12))
m3
#exponential smoothing method
res = Box.test(m3$residuals, lag = 10, type = 'Ljung')
res$parameter
pp = 1- pchisq(res$statistic,res$parameter - 1)
pp
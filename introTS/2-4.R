#2-4
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
tsdiag(m3, gof = 36)
m3
predict(m3, 12)
Box.test(m3$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(lrate)
auto_mode = arima(lrate, order = c(2,1,3))
predict(auto_mode, 4)
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')
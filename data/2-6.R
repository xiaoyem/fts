#2-6
da = read.table("m-aaa-1911.txt", header = T)
aaa = da[,4]
da = read.table("m-baa-1911.txt", header = T)
baa = da[,4]
m1 = lm(diff(aaa) ~ diff(baa))
summary(m1)
par(mfrow = c(2, 1))
#d
m2 = ar(diff(m1$residuals), method='mle')
res = adfTest(m1$residuals,lags=m2$order,type=c("c"))
pvalue = res@test$p.value
if (pvalue > 0.05)
{
  d = 1
}else
{
  d = 0
}
temp = m1$residuals
while (pvalue > 0.05)
{
  temp = diff(temp)
  m3 = ar(diff(temp), method = 'mle')
  res = adfTest(temp,lags=m3$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
#p
pacf(m1$residuals)
p = 2
#q
acf(m1$residuals)
q = 1
m4 = arima(diff(aaa), order = c(p, d, q), xreg = diff(baa))
m4
Box.test(m4$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(diff(aaa))
auto_mode = arima(diff(aaa), order = c(2,0,3))
predict(auto_mode, 4)
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')
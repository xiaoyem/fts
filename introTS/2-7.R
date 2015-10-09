#2-7
da = read.table("q-jnj-earns-9211.txt", header = T)
earns = da[,4]
par(mfrow = c(2, 1))
earns = log(earns)
plot(earns,type = 'o',xlab = 'time', ylab = 'rate')
earns = earns[1:68]
#d
m1 = ar(diff(earns), method='mle')
res = adfTest(earns,lags=m1$order,type=c("c"))
pvalue = res@test$p.value
if (pvalue > 0.05)
{
  d = 1
}else
{
  d = 0
}
temp_rate = earns
while (pvalue > 0.05)
{
  temp_rate = diff(temp_rate)
  m2 = ar(diff(temp_rate), method = 'mle')
  res = adfTest(temp_rate,lags=m2$order,type=c("c"))
  pvalue = res@test$p.value
  pvalue
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
#p
#pacf(earns)
p = 5
#q
#acf(earns)
q = 0

m3 = arima(earns, order = c(p, d, q))
m3
res = predict(m3, 10)
Box.test(m3$residuals, lag = 10, type = 'Ljung')
pre = res$pred
earns1 = c(earns,pre)
plot(earns1,type = 'o',xlab = 'time', ylab = 'rate', col = 'blue')
#using forecast
require(forecast)
auto.arima(earns)
auto_mode = arima(earns, order = c(2,1,0))
predict(auto_mode, 4)
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')
res = predict(auto_mode, 10)
pre = res$pred
earns2 = c(earns,pre)
plot(earns2,type = 'o',xlab = 'time', ylab = 'rate', col = 'blue')

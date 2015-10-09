#2-1
require(fUnitRoots)
da = read.table("m-unrate-4811.txt", header=T)
rate = da[,1]
#(a)
m1 = ar(diff(rate), method='mle')
res = adfTest(rate,lags=m1$order,type=c("c"))
pvalue = res@test$p.value
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
#ARMA(p,d,p)
#d
temp_rate = rate
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
#p according to AIC
pacf(rate)
p = 6
#q according to ACF
acf(rate)
q = 0

m2 = arima(rate, order = c(p,d,q))
predict(m2, 4)
tsdiag(m2, gof = 36)
Box.test(m2$residuals, lag = 12, type = 'Ljung')
m3 = arima(rate, order = c(p,d,q), seasonal = list(order=c(0,1,1),period=12))
predict(m3, 4)
tsdiag(m3, gof = 36)
Box.test(m3$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(rate)
auto_mode = arima(rate, order = c(2,1,2))
predict(auto_mode, 4)
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')
#(c)
#FIXME
jan = rep(c(1,rep(0,11)),63)
jan = c(jan,1,0,0,0,0,0,0,0,0,0,0)
jan
m4 = lm(rate ~ jan)
summary(m4)
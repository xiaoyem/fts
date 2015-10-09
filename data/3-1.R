#3-1
da = read.table("m-CAUS-7611.txt", header = T)
ca = da[,3]
#(a)
#d
m1 = ar(ca, method='mle')
res = adfTest(ca,lags=m1$order,type=c("c"))
pvalue = res@test$p.value
temp = ca
d = 0
while (pvalue > 0.05)
{
  temp = diff(temp)
  m2 = ar(diff(temp), method = 'mle')
  res = adfTest(temp,lags=m2$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
#p according to AIC
pacf(ca)
p = 3
#q according to ACF
acf(ca)
q = 0

m2 = arima(ca, order = c(p,d,q))
Box.test(m2$residuals, lag = 12, type = 'Ljung')
#using forecast
require(forecast)
auto.arima(ca)
auto_mode = arima(ca, order = c(1,2,0))
tsdiag(auto_mode, gof = 36)
Box.test(auto_mode$residuals, lag = 12, type = 'Ljung')


#(b)
us = da[,4]
ca1 = ca[2:429]
us1 = us[1:428]
m3 = lm(ca1 ~ us1)

summary(m3)
#d
m4 = ar(diff(m3$residuals), method='mle')
res = adfTest(m3$residuals,lags=m4$order,type=c("c"))
pvalue = res@test$p.value
if (pvalue > 0.05)
{
  d = 1
}else
{
  d = 0
}
temp = m3$residuals
while (pvalue > 0.05)
{
  temp = diff(temp)
  m5 = ar(diff(temp), method = 'mle')
  res = adfTest(temp,lags=m5$order,type=c("c"))
  pvalue = res@test$p.value
  if (pvalue > 0.05)
  {
    d = d + 1
  }
}
#p
pacf(m3$residuals)
p = 2
#q
acf(m3$residuals)
q = 0

m6 = arima(ca1, order = c(p,d,q), xreg = us1, include.mean = F)
Box.test(m6$residuals, lag = 12, type = 'Ljung')


#(c)
source("backtest.R")
mm1 = backtest(m1, gnp, 385, 1)
mm2 = backtest(m6, ca1, 385, 1, xre = us1)
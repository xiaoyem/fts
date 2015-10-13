#4-4
da = read.table("m-ko-6111.txt", header = T)
ko = da[,2]
ko = log(ko + 1) * 100
#(a)
#TGARCH P173-P174
source('Tgarch11.R')
m1 = Tgarch11(ko)
names(m1)
at = m1$residuals
sigt = m1$volatility
resi = at / sigt
Box.test(resi, lag = 10, type = 'Ljung')
Box.test(resi, lag = 20, type = 'Ljung')
Box.test(resi^2, lag = 10, type = 'Ljung')
Box.test(resi^2, lag = 20, type = 'Ljung')

#(b)
#NGARCH P177-P178
source('Ngarch.R')
m2 = Ngarch(ko)
names(m2)
res = m2$residuals
vol = m2$volatility
resi = res / vol
Box.test(resi, lag = 10, type = 'Ljung')
Box.test(resi^2, lag = 10, type = 'Ljung')

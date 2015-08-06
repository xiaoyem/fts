require(fBasics)
require(timeSeries)

gdp=read.table("q-gdpdef.txt",header=T)
head(gdp)
 def=gdp[,4]
def=as.numeric(def)
ldef=log(def)
 acf(ldef)
 pacf(ldef)
 m1=arima(ldef,order=c(1,0,0))
 tsdiag(m1,gof=36)
predict(m1,4)

#4-6
require(quantmod)
require(xts)
getSymbols("AAPL",from="2007-01-02",to="2011-11-30")
da = coredata(AAPL)
C = da[,4]
O = da[,1]
H = da[,2]
L = da[,3]
rtn = diff(log(da[,6]))

# Yang-Zhang function
Yang_Zhang <- function(start, n, o, c, d, u)
{
  if (start + n > length(o))
  {
    end = length(o)
  }else
  {
    end = start + n
  }
  n = end - start
  sigma_o = 0
  sigma_c = 0
  for (t in start:end)
  {
    sigma_o = sigma_o + (o[t] - mean(o[start:end]))^2
    sigma_c = sigma_c + (c[t] - mean(c[start:end]))^2
  }
  sigma_o = sigma_o / (n-1)
  sigma_c = sigma_c / (n-1)
  temp = u*(u-c) + d*(d-c)
  sigma_rs = mean(temp[start:end])
  k = 0.34 / (1.34 + (n+1) / (n-1))
  sigma_yz = sigma_o + k*sigma_c + (1-k)*sigma_rs
  return (sigma_yz)
}
#(a)
o = log(O[2:length(O)]) - log(C[1:length(C) - 1])
u = log(H) - log(O)
d = log(L) - log(O)
c = log(C) - log(O)

#window 63 (n = 63)
n = 63
win_63 = array(1:length(o))
for (i in 1:ceiling(length(AAPL[,1]) / n))
{
  res = Yang_Zhang((i-1)*n+1, n, o, c, d, u)
  if (i*n > length(o))
  {
    end = length(o)
  }else
  {
    end = i*n
  }
  win_63[((i-1)*n+1):end] = res
}
#win_63
#window 32 (n = 32)
n = 32
win_32 = array(1:length(o))
for (i in 1:ceiling(length(AAPL[,1]) / n))
{
  res = Yang_Zhang((i-1)*n+1, n, o, c, d, u)
  if (i*n > length(o))
  {
    end = length(o)
  }else
  {
    end = i*n
  }
  win_32[((i-1)*n+1):end] = res
}
#win_32
#(b)
require(fGarch)
m1 = garchFit(~1 + garch(1,1), data = rtn, trace = F)
summary(m1)

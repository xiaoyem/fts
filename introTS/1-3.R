#1-3
require(fBasics)
da = read.table("m-ge3dx-4011.txt", header=T)
head(da)
T = length(da[,2])
#(a)
res = t.test(da[,2])
p = res$p.value
if (p > 0.05)
{
  cat("p-value = ",p," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p," <= 0.05, reject the null hypothesis")
}
#(b)
s1 = skewness(da[,2])
t1 = s1 / sqrt(6 / T)
p1 = 2 * (1 - pnorm(t1))
if (p1 > 0.05)
{
  cat("p-value = ",p1," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p1," <= 0.05, reject the null hypothesis")
}
#(c)
s2 = kurtosis(da[,2])
t2 = s2 / sqrt(24 / T)
p2 = 2 * (1 - pnorm(t2))
if (p2 > 0.05)
{
  cat("p-value = ",p2," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p2," <= 0.05, reject the null hypothesis")
}
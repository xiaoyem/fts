#1-4
require(fBasics)
da = read.table("d-axp3dx-0111.txt", header=T)
head(da)
T = length(da[,2])
#(a)
s1 = skewness(log(da[,2] + 1))
t1 = s1 / sqrt(6 / T)
p1 = 2 * (1 - pnorm(t1))
if (p1 > 0.05)
{
  cat("p-value = ",p1," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p1," <= 0.05, reject the null hypothesis")
}
#(b)
s2 = kurtosis(log(da[,2] + 1))
t2 = s2 / sqrt(24 / T)
p2 = 2 * (1 - pnorm(t2))
if (p2 > 0.05)
{
  cat("p-value = ",p2," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p2," <= 0.05, reject the null hypothesis")
}
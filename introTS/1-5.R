#1-5
require(fBasics)
da_uk = read.table("d-fx-ukus-0711.txt", header=T)
da_jp = read.table("d-fx-usjp-0711.txt", header=T)

#(a)
diff_uk = diff(log(da_uk$rate));
diff_jp = diff(log(da_jp$rate));
#(b)
basicStats(diff_uk)
basicStats(diff_jp)
#(c)
plot(density(diff_jp))
#(d)
res = t.test(diff_jp)
p = res$p.value
if (p > 0.05)
{
  cat("p-value = ",p," > 0.05, cannot reject the null hypothesis")
}else
{
  cat("p-value = ",p," <= 0.05, reject the null hypothesis")
}

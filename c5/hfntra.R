"hfntra" <- function(da, int, collapsed = TRUE) {
# Compute number of trades in a given interval (intraday)
#
# int: time intervals in minutes
# da: data in the format: date, hour, minute, second, price, volume
#
	if (!is.matrix(da)) da = as.matrix(da)
	intsec = int * 60
	istart = 9 * 60 * 60 + 30 * 60
	iend = 16 * 60 * 60
	# compute the number of time intervals within a trading day
	tradetime = 6.5 * 60 * 60
	nintval = floor(tradetime / intsec)
	T = dim(da)[1]
	nday = da[T, 1] - da[1, 1] + 1
	## length of the resulting time series
	ntrad = nday * nintval
	des = c(nday, nintval, ntrad)
	names(des) <- c("days", "intervals", "nobs")
	print(des)
	counts = rep(0, ntrad)
	# compute time in seconds from midnight.
	if (collapsed) {
		caltime = da[, 2]
	} else {
		caltime = da[, 2] * 60 * 60 + da[, 3] * 60 + da[, 4]
	}
	#plot(caltime, type = 'l')
	date = da[1, 1]
	for (i in 1:T) {
		if (caltime[i] > (istart - 1)) {
			iday = da[i, 1] - date
			if (caltime[i] < (iend + 1)) {
				if (caltime[i] == istart) {
					counts[iday * nintval + 1] = counts[iday * nintval + 1] + 1
				}
				if (caltime[i] == iend) {
					counts[iday * nintval + nintval] = counts[iday * nintval + nintval] + 1
				}
				if ((caltime[i] > istart) && (caltime[i] < iend)) {
					ii = caltime[i] - istart
					ij = floor(ii / intsec)
					counts[iday * nintval + ij + 1] = counts[iday * nintval + ij + 1] + 1
				}
			}
		}
	}
	# FIXME
	counts = counts[which(counts != 0)]
	par(mfcol = c(2, 1))
	plot(counts, type = 'l')
	title(main = "Time plot of number of transactions")
	acf(counts, lag = 3 * nintval)
	hfntra <- list(ntrad = counts)
}


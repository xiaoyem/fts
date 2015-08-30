"hfrtn" <- function(da, int, logrtn = TRUE, collapsed = TRUE, averaged = TRUE) {
# Compute intraday returns
#
# int: time intervals in minutes
# da: data in the format: date, hour, minute, second, price, volume
#
	if (!is.matrix(da)) da = as.matrix(da)
	intsec = int * 60
	istart = 9 * 60 * 60 + 30 * 60
	iend = 16 * 60 * 60
	# compute the number of prices
	tradetime = 6.5 * 60 * 60
	ntrade = floor(tradetime / intsec)
	T = dim(da)[1]
	nday = da[T, 1] - da[1, 1] + 1
	npri = nday * ntrade
	#print(c(ntrade, nday, npri))
	price = rep(0, npri)
	count = rep(0, npri)
	# price is the last transaction price of the time interval
	if (collapsed) {
		caltime = da[, 2]
	} else {
		caltime = da[, 2] * 60 * 60 + da[, 3] * 60 + da[, 4]
	}
	#plot(caltime, type = 'l')
	date = da[1, 1]
	for (i in 1:T) {
		if (caltime[i] > istart) {
			iday = da[i, 1] - date
			if (caltime[i] < (iend + 1)) {
				if (caltime[i] == iend) {
					if (averaged) {
						if (collapsed) {
							price[iday * ntrade + ntrade] = price[iday * ntrade + ntrade] +
								da[i, 3]
						} else {
							price[iday * ntrade + ntrade] = price[iday * ntrade + ntrade] +
								da[i, 5]
						}
					} else {
						if (collapsed) {
							price[iday * ntrade + ntrade] = da[i, 3]
						} else {
							price[iday * ntrade + ntrade] = da[i, 5]
						}
					}
					count[iday * ntrade + ntrade] = count[iday * ntrade + ntrade] + 1
				}
				if ((caltime[i] > istart) && (caltime[i] < iend)) {
					ii = caltime[i] - istart
					ij = floor(ii / intsec)
					if (averaged) {
						if (collapsed) {
							price[iday * ntrade + ij + 1] = price[iday * ntrade + ij + 1] +
								da[i, 3]
						} else {
							price[iday * ntrade + ij + 1] = price[iday * ntrade + ij + 1] +
								da[i, 5]
						}
					} else {
						if (collapsed) {
							price[iday * ntrade + ij + 1] = da[i, 3]
						} else {
							price[iday * ntrade + ij + 1] = da[i, 5]
						}
					}
					count[iday * ntrade + ij + 1] = count[iday * ntrade + ij + 1] + 1
				}
			}
		}
	}
	if (averaged) {
		for (i in 1:npri) {
			if (count[i] > 0) price[i] = price[i] / count[i]
		}
	}
	for (i in 2:npri) {
		if (price[i] <= 0) price[i] = price[i - 1]
	}
	#plot(price, type = 'l')
	pri = log(price)
	#skip overnight returns
	nrtn = ntrade - 1
	rtn = NULL
	for (i in 1:nday) {
		ist = (i - 1) * ntrade
		for (j in 2:ntrade) {
			rtn = c(rtn, pri[ist + j] - pri[ist + j - 1])
		}
	}
	hfrtn <- list(rtn = rtn, price = price)
}


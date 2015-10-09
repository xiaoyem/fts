#2-3
require(quantmod)
getSymbols("AAPL",from="2007-01-02",to="2011-12-23")
high = AAPL$AAPL.High
low = AAPL$AAPL.Low
dif = high - low
acf(dif, lag = 100)
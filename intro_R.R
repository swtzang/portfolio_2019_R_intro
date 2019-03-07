install.packages("quantmod")
library(quantmod)
getSymbols("GOOG", auto.assign = TRUE)
# There are two primary time series data
# xts and zoo
tickers = c("GOOG", "MSFT", "AMZN")
getSymbols(tickers, from = "2010-01-01", auto.assign = TRUE)
data = new.env()
getSymbols(tickers, from = "2010-01-01", env = data , auto.assign = TRUE)
#
str(AMZN)
class(AMZN)
tail(AMZN,3)
names(AMZN)
ls(AMZN)
head(AMZN$AMZN.Close)
AMZN2010_15=AMZN['2010/2015']
#












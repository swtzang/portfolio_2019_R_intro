rm(list=ls())
#install.packages("quantmod")
library(quantmod)
getSymbols("GOOG", auto.assign = TRUE)
# There are two primary time series data
# xts and zoo
tickers = c("GOOG", "MSFT", "AMZN")
getSymbols(tickers, from = "2010-01-01", auto.assign = TRUE)
# download multiple prices to data variable
data = new.env()
getSymbols(tickers, from = "2010-01-01", env = data , auto.assign = TRUE)
ls(data)
names(data)
head(data$AMZN)
#
str(AMZN)
class(AMZN)
tail(AMZN,3)
names(AMZN)
ls(AMZN)
head(AMZN$AMZN.Close)
AMZN2010_15=AMZN['2010/2015']
#Extract closing price: Cl(), 
# Adjusted price: Ad()
AMZN.ad<-Ad(AMZN)
head(AMZN.ad)
class(AMZN.ad)
# write function to gather adjusted prices together
firm3<-merge(Ad(AMZN), Ad(GOOG), Ad(MSFT))
head(firm3)
colnames(firm3)<-c("AMZN", "GOOG", "MSFG")
head(firm3)
# write function to compute returns
library(magrittr)
simple_returns01<-function(x) {
      coredata(x[-1,])/coredata(x[-length(x),]) - 1 %>% 
      na.omit()
}
head(simple_returns01(AMZN.ad))
# or to keep the index of dates
simple_returns02<-function(x) {
  na.omit(x/lag(x) - 1)
}
head(simple_returns02(AMZN.ad))
#
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
head(na.omit(Return.calculate(firm3)))
firm3.mon<-firm3 %>% to.monthly(indexAt = "lastof", OHLC=FALSE) %>% 
                 Return.calculate()
head(firm3.mon)
# Main problem: the return series are shown in calendar dates, 
# not in trading dates
# Therefore, we use package PMwR instead!
#install.packages("PMwR")
library(PMwR)
head(returns(firm3))
firm3.mon1<-returns(firm3, period = "month")
#options(digits = 5)
head(firm3.mon1)
firm3.mon1
firm3.day<-returns(firm3)
head(merge(firm3, firm3.day))
head(100* cumprod(1+firm3.ret))
firm3.p.ret<-firm3.day %>% merge(firm3)  
             

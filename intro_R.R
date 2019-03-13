rm(list=ls())
#install.packages("quantmod")
# Import data from yahoo finance
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
#------------------------------------------
# Import data from .txt file
etf4<-read.table("ETF4_2010_2018_d1_english.txt", header = T, sep = ',')
str(etf4)
# convert CO_ID into character;
etf4<-read.table("ETF4_2010_2018_d1_english.txt", header = T, sep = ',',
                 colClasses = c("CO_ID" = "character"))
str(etf4)
head(etf4)
write.csv(etf4, "etf4.csv")
#
installed.packages('readr')
library(readr)
etf4_csv<-read_csv("etf4.csv")
etf4_csv
#--------------------------
# clean data
etf4.c<-etf4_csv[, c(-1,-3,-6)]
etf4.c
colnames(etf4.c)<-c("id", "date", "price")
etf4.c
# use pipe operator 
library(magrittr)
#install.packages("dplyr")
library(dplyr)
etf4.c<-etf4_csv%>%select(c(2,4,5))%>%rename("id" = "CO_ID", "date"= "Date", "price" = "Close")
etf4.c
#-----------------------------------------------------------------                
# use dcast to reorder dataframe by date;
#install.packages("reshape2")
library(reshape2)
etf4.reorder = dcast(etf4.c, date~id)
dim(etf4.reorder)
head(etf4.reorder)
str(etf4.reorder)
# convert into date format using as.Date()
etf4.reorder$date<-as.Date(as.character(etf4.reorder$date), "%Y%m%d") 
head(etf4.reorder)
str(etf4.reorder)
# convert character into numeric 
# convert to xts
#install.packages("xts")
library(xts)
etf4.xts<-xts(etf4.reorder[,-1], order.by = etf4.reorder$date)
head(etf4.xts)
tail(etf4.xts)
str(etf4.xts)
#----------------------------------------------
# Handling missingness in your data 
#----------------------------------------------
# Last obs. carried forward
#etf4.xts$`0050`['2018-12-27']<-NA 
#tail(etf4.xts)
etf4.xts<-na.locf(etf4.xts)                
tail(etf4.xts)
# Next obs. carried backward
etf4.xts.fill<-na.locf(etf4.xts, fromLast = TRUE) 
head(etf4.xts.fill)
#-------------------------------------------------
# delete NA values
etf4.xts<-na.omit(etf4.xts)
head(etf4.xts)
# or complete cases
#install.packages("tidyr")
library(tidyr)
etf4.xts1<-etf4.xts[complete.cases(etf4.xts),]
head(etf4.xts1)
#-----------------------------------------------------------
# export data
#----------------------------------------------------------
write.csv(etf4.xts, file = "myetf4.csv")
# date index disappears!!!
# you have to use write.zoo to save .xts file
write.zoo(etf4.xts, sep = ',', file = "myetf4.csv.1")
# you can save big file using saveRDS()
saveRDS(etf4.xts, file = "etf4.xts.rds")
etf4.xts2 <- readRDS("etf4.xts.rds")
head(etf4.xts2)
##
etf4.zoo <- read.zoo("myetf4.csv.1", header = TRUE, index.column =1, 
                     sep = ",", format = "%Y-%m-%d")
head(etf4.zoo)
class(etf4.zoo)
etf4.xts3<-as.xts(etf4.zoo)
head(etf4.xts3)
#=============================================
# Querying for data
#=============================================
etf4_2016<-etf4.xts['2016']
etf4_2016_01_06 <- etf4.xts["20160101/20160630"]
head(etf4_2016_01_06)

#-----------------------------------------
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



             

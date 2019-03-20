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
colnames(firm3)<-c("AMZN", "GOOG", "MSFT")
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
#installed.packages('readr')
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
# We can also use package PMwR instead!
# install.packages("PMwR")
library(PMwR)
firm3.day<-returns(firm3, pad=0)
head(firm3.day)
firm3.mon1<-returns(firm3, period = "month")
#options(digits = 5)
head(firm3.mon1)
firm3.mon1
head(merge(firm3, firm3.day))
cum.ret<-cumprod(1+firm3.day)
head(cum.ret)
tail(cum.ret)
Return.cumulative(firm3.day)

# merge daily return and prices 
firm3.p.ret<-firm3.day %>% merge(firm3) 
colnames(firm3.p.ret)<-c("AMZN_ret", "GOOG_ret", "MSFT_ret", 
                         "AMZN_price", "GOOG_price", "MSFT_price")
head(firm3.p.ret)
#============================================================
# Plot in R
# Reference:
# https://blog.revolutionanalytics.com/2014/01/quantitative-finance-applications-in-r-plotting-xts-time-series.html
#-------------------------------------------------------------
# Using plot(.) in the zoo package
plot(cum.ret)
# Set a color scheme:
tsRainbow <- rainbow(ncol(cum.ret))
# Plot the overlayed series
plot(x = cum.ret, ylab = "Cumulative Return", main = "Cumulative Returns",
     col = tsRainbow, screens = 1, lty = 1:3)
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topleft", legend = c("AMZN", "GOOG", "MSFT"), 
       lty = 1:3,col = tsRainbow)
# use your own customized color set
myColors <- c("red", "darkgreen", "goldenrod")
plot(x = cum.ret, ylab = "Cumulative Return", main = "Cumulative Returns",
     col = myColors, screens = 1, lty =1:3, lwd = 2)
legend(x = "topleft", legend = c("AMZN", "GOOG", "MSFT"), 
       lty = 1:3,col = myColors)
#---------------------------------------------------------------
# Using plot(.) in the xts package
# Reference: 
# https://timelyportfolio.blogspot.com/2012/08/plotxts-is-wonderful.html
#----------------------------------------------------------------
cum.ret.xts<-as.xts(cum.ret)
cum.ret.xts
plot(cum.ret.xts, xlab = "Time", ylab = "Cumulative Return",
     main = "Cumulative Returns", ylim = c(0.0, 16), 
     major.ticks= "years",
     minor.ticks = FALSE, col = myColors,
     legend = c("AMZN", "GOOG", "MSFT"))
# not working by adding legend
plot.xts(cum.ret.xts, lwd = 2, legend.loc = "topleft", auto.legend=TRUE,
         main="Cumulative returns")
#
plot.xts(cum.ret.xts, 
         lwd = 2, legend.loc = "topleft", auto.legend=TRUE,
         main="Cumulative returns")
#---------------------------------------------------------------
# plot daily returns
class(firm3.day)
head(firm3.day)
plot(firm3.day)
#
x0 = index(firm3.day)
y0 = coredata(firm3.day)
plot(x0, y0[,1], type = "l")
plot(index(firm3.day), firm3.day$AMZN, type = "l", 
     main="Amazon daily returns", col.main="red", 
     sub="201001-201903", col.sub="blue", 
     xlab="date", ylab="Amazon daily returns",
     col.lab="black", cex.lab=0.75)

# plot xts object
firm3.day %>% as.xts %>% 
              plot
axis(1, index(firm3.day), format(index(firm3.day), "%Y/%m"))
# 
firm3.day.xts <-as.xts(firm3.day)
plot.xts(firm3.day.xts, auto.legend = TRUE)
#axis(side=1, at=yahoo2$date[ at ], labels=format(yahoo2$date[at], '%b-%y'))
#plot.xts(etf4_mon_ret, auto.legend = TRUE)

# plot the scatterplot of AMZN and MSFT
# convert xts into df 
library(ggplot2)
firm3_ret.df1<-fortify(firm3_ret.df)
plot(firm3_ret.df1$`0050`, etf4_returns.df1$`00646`, pch=20,
     col = 'darkred', main = '0050 vs. 00646 monthly returns',
     xlab = '0050', ylab = '00646 S&P500')
#-----------------------------------------------------------
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
# convert xts into data frame which can be used by ggplot
#etf4_returns.df<-fortify(etf4_returns_xts)
firm3_ret.df<-fortify(firm3.day, melt=TRUE)
head(firm3_ret.df)
#
p<-ggplot(firm3_ret.df, aes(x = Index, y = Value))+
  geom_line(aes(color = Series), size = 0.5)
p
p + scale_x_date(date_labels = "%Y/%m")

# histogram distribution
q<-firm3_ret.df %>%
  ggplot(aes(x =Value, fill = Series)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Daily Returns")
q
q + facet_wrap(~Series)+ theme_update(plot.title = element_text(hjust = 0.5))


# line distribution
firm3_ret.df %>%
  ggplot(aes(x = Value, colour = Series)) +
  geom_density(alpha = 1) +
  ggtitle("Daily Returns Density from 2010") +
  xlab("daily returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# Combine line and histogram together
firm3_ret.df %>%
  ggplot(aes(x = Value)) +
  geom_density(aes(color = Series), alpha = 1) +
  geom_histogram(aes(fill = Series), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~Series) +
  ggtitle("Daily Returns from 2010") +
  xlab("daily returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))



             

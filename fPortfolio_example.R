# *********************************************
# Application of fPortfolio package
# Ref: https://www.youtube.com/watch?v=pchqBhof62U
#
library(fPortfolio)
library(timeSeries)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

# Import data
firm_data1 = read.csv('3firmExample_data3.csv')
str(firm_data1)
firm_data1$date
# Change the class of firm_darta1 from a dataframe to a time series 
return.matrix <- as.timeSeries(firm_data1[, -1])
class(return.matrix)
# [1] "timeSeries"
# attr(,"package")
# [1] "timeSeries"

# Another way to convert data into time series
library(xts)
date1 = as.Date(firm_data1[,1], "%Y/%m/%d")
ret.ts<- timeSeries(firm_data1[, -1], date1)
head(ret.ts)
class(ret.ts)
chart.CumReturns(ret.ts, legend.loc = 'topleft', main = '')
#
efficient.frontier = portfolioFrontier(
                         return.matrix, 
                         `setRiskFreeRate<-` (portfolioSpec(), 0.06/12), 
                         constraints = "LongOnly")

# plot(portfolioFrontier(return.matrix))

# Make a plot selection (or 0 to exit): 
#   
# 1:   Plot Efficient Frontier
# 2:   Add Minimum Risk Portfolio
# 3:   Add Tangency Portfolio
# 4:   Add Risk/Return of Single Assets
# 5:   Add Equal Weights Portfolio
# 6:   Add Two Asset Frontiers [LongOnly Only]
# 7:   Add Monte Carlo Portfolios
# 8:   Add Sharpe Ratio [Markowitz PF Only]                         

plot(efficient.frontier, c(1, 2, 3, 4))
plot(efficient.frontier, c(7))
plot(efficient.frontier, c(1, 7))
plot(efficient.frontier, c(1, 3, 7))

# Minimum variance and optimal portfolios
Spec = portfolioSpec()
portfolio.min.var = minvariancePortfolio(
                     return.matrix, 
                     `setRiskFreeRate<-` (portfolioSpec(), 0.06/12))
weights.min.var = getWeights(portfolio.min.var)
weights.min.var
barplot(weights.min.var)
pie(weights.min.var)

# Tangency portfolio
optimal.portfolio = tangencyPortfolio(
                      return.matrix, 
                      `setRiskFreeRate<-` (portfolioSpec(), 0.01/12),
                      constraints = "Short"
)

weights.optimal.port = getWeights(optimal.portfolio)  
weights.optimal.port  
  
  
  






















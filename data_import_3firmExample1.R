# 3-asset example
#
#
#getwd()


#install.packages("gdata", dependencies = TRUE)
#=======================================
# Step 1: Import data from excel
#=======================================
# If data has percentage format, then you'd better convert it into numeric format in excel
# file = 3firmExample_data.csv
# First you have to convert factor data into numeric
#===========================================================================================================     
# http://stackoverflow.com/questions/28977777/r-converting-data-frame-of-percentages-from-factor-to-numeric
#==========================================================================================================
# Delete % from data and convert into numeric
#==========================================================================================================
df2<-data.frame(lapply(firm_data[,-1], function(x) as.numeric(sub("%", "", x))) )
df2 = df2/100
m.ret = colMeans(df2)     
vars = var(df2) 
stdi = sqrt(diag(vars))

rf = 0.005

#==========================================================================================================
# Import data from csv with numeric data without percentage; 
#=======================================================================================
#install.packages("XLConnect", dependencies=TRUE)
# point to 64-bit java location
options(java.home="C:\\Program Files\\Java\\jre1.8.0_65")
library(rJava)
library(XLConnect)
install.packages(pkgs="XLConnect")
install.packages(pkgs="XLConnectJars")
library(XLConnectJars)
library(XLConnect)


install.packages(pkgs="gdata")
library(gdata)

firm_data2 = read.xls('D:/?Ȭw?j?ǤW?Ҹ???/Portfolio management 2016 Spring/3firmExample_data1.xlsx')
firm_data1 = read.csv('D:/?Ȭw?j?ǤW?Ҹ???/Portfolio management 2016 Spring/3firmExample_data1.csv',
                      stringsAsFactors = FALSE)
firm_data1 = read.table('D:/?Ȭw?j?ǤW?Ҹ???/Portfolio management 2016 Spring/3firmExample_data1.csv', sep=",", header=TRUE)
firm_data1 = readWorksheetFromFile('D:/?Ȭw?j?ǤW?Ҹ???/Portfolio management 2016 Spring/3firmExample_data2.xlsx', 
                                    sheet = 1, startRow = 1, endCol = 4)
############################################
#Change system time to English so that we can change date format smoothly
############################################
Sys.getlocale(category = "LC_TIME")
Sys.setlocale("LC_TIME", "Chinese")

date2 = firm_data2[,1]
date22<- paste(substr(date2,1,3),"-01-",substr(date2,5,6),sep="")
datetoformat <- as.Date(date11,"%b-%d-%y")
#convert firm_data2 into time series data
firm_data2.xts = as.xts(firm_data2[,-1], order.by = datetoformat)

date11<- paste("01-",substr(date1,1,3), "-", substr(date1,5,6),sep="")
datetoformat <- as.Date(date11,"%d-%b-%y")
data$date <- as.Date(firm_data1$date, "%b-%y")

#===============================================================================================================
# Before importing data, we first change date format to yyyy/mm/dd and save data file as 3firmExample_data3.csv
#===============================================================================================================
firm_data3 = read.csv('D:/?Ȭw?j?ǤW?Ҹ???/Portfolio management 2016 Spring/3firmExample_data3.csv')
date3 = as.Date(firm_data3[,1], "%Y/%m/%d")
#convert firm_data3 into time series data
firm_data3.xts = as.xts(firm_data3[,-1], order.by = date3)
#=================================
# Minimum variance portfolio
#=================================
Sigma = cov(firm_data1[,2:4])
std = sqrt(diag(Sigma))
ones = rep(1,3)     
one.vec = matrix(ones, ncol=1)
a = inv(Sigma)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w

#====================================================================================================
# Or write your own function to find min var for a  specified return mu;
# Reference: Introduction to R for Quantitative Finance: Chapter 2, p31
# Solve a diverse range of problems with R, one of the most powerful tools for quantitative finance
#====================================================================================================
return = firm_data1[,2:4]

minvariance <- function(return, mu = 0.005) {
  #return <- log(tail(assets, -1) / head(assets, -1))
  Ax <- rbind(2*cov(return), colMeans(return), rep(1, ncol(return)))
  Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
  b0 <- c(rep(0, ncol(return)), mu, 1)
  solve(Ax, b0)
}

#======================================================
# Create frontier function to plot efficient frontier
#======================================================

frontier <- function(return) {
  #return <- log(tail(assets, -1) / head(assets, -1))
  n = ncol(return)
  Q = cov(return)
  Ax <- rbind(2*cov(return), colMeans(return), rep(1, n))
  Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
  r <- colMeans(return)
  rbase <- seq(min(r), max(r), length = 100)
  s <- sapply(rbase, function(x) {
    b0 <- c(rep(0, ncol(return)), x, 1)
    y <- head(solve(Ax, b0), n)
    sqrt(y%*%Q%*%y)
  })
  plot(s, rbase, xlab = 'Std', ylab = 'Return')
}

frontier(return)

#===============================================
# Use package fPortfolio to plot frontier
#===============================================
library(timeSeries)
library(PerformanceAnalytics)
#install.packages("rugarch", dependencies=TRUE)
#install.packages("PerformanceAnalytics", dependencies=TRUE)
#install.packages("fAssets", dependencies=TRUE)
#install.packages("fPortfolio",dependencies=TRUE)

library(fPortfolio)

# convert data to timeseries
ret.ts<- timeSeries(return, firm_data1[,1])
chart.CumReturns(ret.ts, legend.loc = 'topleft', main = '')


plot(portfolioFrontier(ret.ts))

#To mimic what we have implemented in the preceding code, let us render the
#Frontier plot of short sale constraints

Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
# Display structure of constraints;
portfolioConstraints(ret.ts, Spec, constraints = "Short")
#setSolver(Spec) = "solveRquadprog"
Frontier <- portfolioFrontier(as.timeSeries(ret.ts), Spec, constraints = "Short") # "LongOnly"
frontierPlot(Frontier, col = rep('orange', 2), pch = 19)
monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
grid()

#=====================================================
# Tangency portfolio with risk free rate = 0
#======================================================
tgPortfolio <- tangencyPortfolio(ret.ts, Spec, constraints = "Short")




#=============================================
# Global min var portfolio
#=============================================
globminSpec <- portfolioSpec()
# Don't change the constraints to "Short" as there will not be any results!
globminPortfolio <- minvariancePortfolio(as.timeSeries(ret.ts), spec = globminSpec, constraints = "LongOnly")
print(globminPortfolio)

col = rampPalette(ncol(ret.ts), "purple2green")
weights <- 100 * as.vector(getWeights(globminPortfolio))
names <- as.vector(names(ret.ts))
barplot(height = weights, names.arg = names,
          horiz = TRUE, las = 1, col = col)
title(main = "Weights of Global Min Variance Portfolio",
        xlab = "Weights %")



#=================================
# Tangency portfolio
#=================================
rf = 0.00
mu = colMeans(firm_data1[,2:4])
mu.mtx = matrix(mu, ncol=1)
mu_rf = mu - rf
mu_rf = matrix(mu_rf, ncol=1)
a1 = inv(Sigma)%*%mu_rf
b1 = t(one.vec)%*%a1
tp = a1 / as.numeric(b1)
#portfolio expected return
ret.tp  = sum(mu.mtx*tp)
#portfolio standard deviation
std.tp = sqrt((t(tp)%*%Sigma)%*%tp)
#sharpe ratio
sharpe.tp = (ret.tp - rf)/std.tp

#=================================
# quadratic programming
#=================================
library(quadprog)

Amat = cbind(rep(1,3),mu)  # set the constraints matrix

muP = seq(.01,.08,length=300)  # set of 300 possible target values 
# for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*Sigma,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

postscript("D:\\?Ȭw?j?ǤW?Ҹ???\\Portfolio management 2015 Fall\\3firmExample.ps",width=6,height=5)  
plot(sdP,muP,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 0.005/12 # input value of risk-free interest rate
points(0,mufree,cex=3,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=2,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=1,pch=19, col="red") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+", col="blue") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.08),lwd=1)  #  plot the efficient frontier
points(c(std[1],std[2], std[3]), c(mu[1], mu[2], mu[3]), cex=1, pch="o", col="red") 
text(std[1],mu[1],"Nordstrom",cex=1, pos=4)
text(std[2],mu[2],"Starbucks",cex=1, pos=4)
text(std[3],mu[3],"Microsoft",cex=1, pos=4)
graphics.off()

#===================================
# no short sales constraint
#===================================
Amat1 = cbind(rep(1,3),mu, diag(1,nrow=3))  # set the constraints matrix

# muP = seq(.01,.08,length=300)  # set of 300 possible target values 
# When short sales are prohibited, the target expected return on the portfolio must lie between the smallest 
# and largest expected returns on the stocks. 
muP1 = seq(min(mu)+.0001,max(mu)-.0001,length=300)

# for the expect portfolio return
sdP1 = muP1 # set up storage for standard deviations of portfolio returns
weights1 = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

i=1
for (i in 1:length(muP1))  # find the optimal portfolios for each target expected return
{
  bvec1 = c(1,muP1[i], rep(0,3))  # constraint vector
  result = solve.QP(Dmat=2*Sigma,dvec=rep(0,3),Amat=Amat1,bvec=bvec1,meq=2)
  sdP1[i] = sqrt(result$value)
  weights1[i,] = result$solution
}

postscript("D:\\?Ȭw?j?ǤW?Ҹ???\\Portfolio management 2015 Fall\\3firmExample_noshort.ps",width=6,height=5)  
plot(sdP1,muP1,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1)  #  plot 
#the efficient frontier (and inefficient frontier)
par(new=TRUE)
plot(sdP,muP,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1, col="green")  #  plot 
mufree = 0.005 # input value of risk-free interest rate
points(0,mufree,cex=3,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=2,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=1,pch=19, col="red") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+", col="blue") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.08),lwd=1)  #  plot the efficient frontier
points(c(std[1],std[2], std[3]), c(mu[1], mu[2], mu[3]), cex=1, pch="o", col="red") 
text(std[1],mu[1],"Nordstrom",cex=1, pos=4)
text(std[2],mu[2],"Starbucks",cex=1, pos=4)
text(std[3],mu[3],"Microsoft",cex=1, pos=4)
graphics.off()


#====================================================
# David Ruppert's example in his textbook
#===================================================
install.packages("Ecdat",dependencies=TRUE,repos="http://ftp.yzu.edu.tw/CRAN/")
install.packages("Ecfun", dependencies = TRUE, repos="http://ftp.yzu.edu.tw/CRAN/")
library(Ecfun)
library(Ecdat)
library(quadprog)

data(CRSPday)
#daily observations from 1969-1-03 to 1998-12-31
#number of observations : 2528
#ge the return for General Electric, Permno 12060
#ibm the return for IBM, Permno 12490
#mobil the return for Mobil Corporation, Permno 15966
#crsp the return for the CRSP value-weighted index, including dividends

class(CRSPday)
CRSP.df = as.data.frame(CRSPday)

R = 100*CRSP.df[,4:6]
mean_vect = apply(R,2,mean)
cov_mat = cov(R)
sd_vect = sqrt(diag(cov_mat))

Amat = cbind(rep(1,3),mean_vect) 

muP = seq(.05,.14,length=300)  # set of 300 possible target values 
# for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

postscript("D:\\?Ȭw?j?ǤW?Ҹ???\\Portfolio management 2015 Fall\\CRSP_3firm.ps",width=6,height=5)  #  Figure 11.3
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.15),lty=3)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 1.3/253 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=4,lty=2)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*", col='blue') # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
points(c(sd_vect[1],sd_vect[2], sd_vect[3]), c(mean_vect[1], mean_vect[2], mean_vect[3]), cex=1, pch="o", col="red") # show tangency portfolio
text(sd_vect[1],mean_vect[1],"GE",cex=1, pos=4)
text(sd_vect[2],mean_vect[2],"IBM",cex=1, pos=4)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1, pos=4)
graphics.off()










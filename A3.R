####Preamble####
library(stats)
library(tseries)
library(forecast)

####Question 1####

#Import Data
setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)

#Part A

adf.test(dollar,k=1)
adf.test(dollar,k=2)
adf.test(dollar,k=3)
adf.test(dollar,k=4)
adf.test(dollar,k=5)
adf.test(dollar,k=6)
adf.test(dollar,k=7)
adf.test(dollar,k=8)
adf.test(dollar,k=9)

#Part B

source("bartlett.txt")
bartlett(returns)
Box.test(returns,lag=10,type="Ljung")
Box.test(returns,lag=9,type="Ljung")


#### QUESTION 2 ####

install.packages("‘normwhn.test")

yield <- scan(file="yield.txt")
yield <- ts(yield)
View(yield)
ddollar <- diff(dollar)
View(ddollar)

#Part A

Box.test(ddollar,type="Ljung")
Box.test(ddollar,lag=9,type="Ljung")

#Part B

z<-arima(dollar, c(1, 1, 1))
z$aic
z<-arima(dollar, c(2, 1, 2))
z$aic
z<-arima(dollar, c(3, 1, 3))
z$aic

#Part C
z<-arima(dollar, c(1, 1, 2))
z$aic
z<-arima(dollar, c(1, 1, 3))
z$aic
y<-arima(dollar, c(2, 1, 1))
y$aic
z<-arima(dollar, c(2, 1, 3))
z$aic
z<-arima(dollar, c(3, 1, 1))
z$aic
z<-arima(dollar, c(3, 1, 2))
z$aic

auto.arima(dollar)

#Part D
Box.test(y$residuals)
tsdiag(y)


####QUESTION 3 ####


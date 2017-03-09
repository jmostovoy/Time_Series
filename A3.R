####Preamble####
library(stats)
library(tseries)

####Question 1####

#Import Data
setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)

#Part a

adf.test(dollar,k=1)
adf.test(dollar,k=2)
adf.test(dollar,k=3)
adf.test(dollar,k=4)
adf.test(dollar,k=5)
adf.test(dollar,k=6)
adf.test(dollar,k=7)
adf.test(dollar,k=8)
adf.test(dollar,k=9)

#Part b

source("bartlett.txt")
bartlett(returns)
Box.test(returns,lag=10,type="Ljung")
Box.test(returns,lag=9,type="Ljung")

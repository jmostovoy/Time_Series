#A4
### Preamble #############################
library(stats)
library(tseries)
library(forecast)
library(xtable)

### Question 1 #############################

#Import Data
setwd("~/Documents/Time_Series")
fatal <- scan(file="fatalities.txt")
fatal <- ts(log(fatal), start=c(1960,1),end=c(1974,12),freq=12)
View(fatal)

# Part A =================================
r <- stl(fatal,s.window = 3 , t.window = 51)

plot(r)

r <- stl(fatal,s.window = 5, t.window = 61)

plot(r)

r <- stl(fatal,s.window = "periodic", t.window = 41)
plot(r)

# Part B =================================
# Part C =================================

### Question 2 #############################
speech <- ts(scan(file="fatalities.txt"),frequency=10000)
source("spec.parzen.txt")

# Part A =================================
r <- spec.parzen(speech,maxlag=60,plot=T)
# Part B =================================
r <- spec.ar(speech,order=10,method="burg")
# Part C =================================
### Question 3 #############################
gold<-ts(scan(file="barrick.txt"))
# Part A =================================
# Part B =================================
# Part C =================================

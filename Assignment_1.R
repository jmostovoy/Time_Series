#Preamble:

library(MASS)
library(moments)
library(car)
library(zoo)
library(lmtest)
library(sandwich)
library(strucchange)
library(urca)
library(vars)
library(lmtest)
library(vars)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)
library(stats)

#Question 1

setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)


acf(dollar)
spec.pgram(dollar)
acf(returns)
spec.pgram(returns)


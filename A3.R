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

fatalities <- scan(file="fatalities.txt")
fatalities <- ts(fatalities)
View(fatalities)

#Part A

#(i)
ddfatalities <- diff(diff(fatalities))
View(ddfatalities)

#(ii)

acf(ddfatalities)
pacf(ddfatalities)

#implies MA(3) - I think...

#(iii)
auto.arima(ddfatalities)
g<-arima(ddfatalities)

#(iv)
Box.test(g$residuals)
tsdiag(g)

#Part B

g1 <- predict(g,n.ahead=12)
g1$pred
g1$se

#Part C

subfatalities<-fatalities[c(1:168)]
ddsubfatalities <- diff(diff(subfatalities))
View(ddsubfatalities)
acf(ddsubfatalities)
pacf(ddsubfatalities)
auto.arima(ddsubfatalities)
h<-arima(ddsubfatalities)
Box.test(h$residuals)
tsdiag(h)
h1 <- predict(h,n.ahead=12)
h1$pred
h1$se




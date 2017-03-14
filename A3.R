####Preamble####
library(stats)
library(tseries)
library(forecast)
install.packages("fpp")
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
adf.test(dollar,k=10)
adf.test(dollar,k=11)
adf.test(dollar,k=12)
x<-adf.test(dollar,k=13)

#Part B

source("bartlett.txt")
bartlett(returns)
Box.test(returns,lag=10,type="Ljung")
Box.test(returns,lag=9,type="Ljung")
Box.test(returns,lag=20,type="Ljung")
Box.test(returns,lag=40,type="Ljung")



#### QUESTION 2 ####

yield <- scan(file="yield.txt")
yield <- ts(yield)
View(yield)
dyield <- diff(yield)
View(dyield)

#Part A

Box.test(dyield,type="Ljung")
Box.test(dyield,lag=9,type="Ljung")
bartlett(dyield)
acf(dyield)
pacf(dyield)
arima.yield <- arima(yield, order=c(0,1,0))
acf(arima.yield$residuals)
bartlett(arima.yield$residuals)
Box.test(arima.yield$residuals,lag=10,type="Ljung")


#Part B

z<-arima(yield, c(1, 1, 1))
z$aic
z<-arima(yield, c(2, 1, 2))
z$aic
z<-arima(yield, c(3, 1, 3))
z$aic

#Part C
z<-arima(yield, c(0, 1, 0))
z$aic
z<-arima(yield, c(0, 1, 1))
z$aic
y<-arima(yield, c(0, 1, 2))
y$aic
y<-arima(yield, c(0, 1, 3))
y$aic
y<-arima(yield, c(0, 1, 4))
y$aic
z<-arima(yield, c(1, 1, 0))
z$aic
z<-arima(yield, c(1, 1, 2))
z$aic
z<-arima(yield, c(1, 1, 3))
z$aic
z<-arima(yield, c(1, 1, 4))
z$aic
y<-arima(yield, c(2, 1, 1))
y$aic
z<-arima(yield, c(2, 1, 3))
z$aic
z<-arima(yield, c(3, 1, 0))
z$aic
z<-arima(yield, c(3, 1, 1))
z$aic
z<-arima(yield, c(3, 1, 2))
z$aic
z<-arima(yield, c(3, 1, 4))
z$aic
z<-arima(yield, c(4, 1, 0))
z$aic
z<-arima(yield, c(4, 1, 1))
z$aic
z<-arima(yield, c(4, 1, 2))
z$aic
z<-arima(yield, c(4, 1, 3))
z$aic
z<-arima(yield, c(4, 1, 4))
z$aic
z<-arima(yield, c(6, 1, 6))
z$aic


z<-data.frame(c(1:5),c(1:5),c(1:5),c(1:5),c(1:5))
for (i in c(1:5)) {
  for (j in c(1:5)) {
    z[i,j]<-arima(yield, c(i, 1, j))$aic
  }
}
z

auto.arima(yield)

#Part D

Box.test(arima(yield,c(3, 1, 3))$residuals, lag=10)
bartlett(arima(yield,c(3, 1, 3))$residuals)
acf(arima(yield,c(3, 1, 3))$residuals)
pacf(arima(yield,c(3, 1, 3))$residuals)
tsdiag(arima(yield,c(3, 1, 3)))


####QUESTION 3 ####

fatalities <- scan(file="fatalities.txt")
lfatalities <- ts(log(fatalities))
View(lfatalities)


#Part A

#(i)
dlfatalities <- diff(diff(lfatalities,lag=12), lag=1)
View(dlfatalities)

#(ii)

acf(dlfatalities, lag.max = 40)
pacf(dlfatalities,  lag.max = 40)
Arima(fatalities, order=c(0,1,1), seasonal=list(order = c(0, 1, 1), period = 12))$aic
Arima(fatalities, order=c(1,1,1), seasonal=list(order = c(1, 1, 1), period = 12))$aic
Arima(fatalities, order=c(0,1,2), seasonal=list(order = c(0, 1, 2), period = 12))$aic
Arima(fatalities, order=c(1,1,2), seasonal=list(order = c(1, 1, 2), period = 12))$aic

#test
fit3 <- Arima(fatalities, order=c(0,1,1), seasonal=list(order = c(0, 1, 1), period = 12))
res <- residuals(fit3)
tsdisplay(res)
Box.test(res, lag=12, fitdf=12, type="Ljung")


fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
res <- residuals(fit3)
tsdisplay(res)
Box.test(res, lag=16, fitdf=4, type="Ljung")

data(euretail, package = "fpp")

#implies MA(3) - I think...

#(iii)
auto.arima(dlfatalities)
g<-arima(dlfatalities)
acf(arima(dlfatalities, order = c(0, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 12))$residuals)
arima(dlfatalities, order = c(1, 1, 1),
      seasonal = list(order = c(1, 1, 1), period = 12))$aic
sarima(dlfatalities,0,1,1,0,1,1,12)

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




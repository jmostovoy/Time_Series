# Section One ---------------------------------

# Section Two =================================

### Section Three ############################# 

####Preamble####
library(stats)
library(tseries)
library(forecast)
install.packages("fpp")
library(forecast)
library(xtable)

####Question 1####

#Import Data
setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)

#Part A
adfdollar_pvalue<-c(1:20)
for (i in c(1:20)) {
  adfdollar_pvalue[i]<-adf.test(dollar,k=i)$p.value
}
adfdollar_pvalue

xtable(t(as.data.frame(adfdollar_pvalue)), digits = 3)

#Part B

source("bartlett.txt")
bartlett(returns)
boxreturns_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxreturns_pvalue[i]<-Box.test(returns,lag=i,type="Ljung")$p.value
}
boxreturns_pvalue
xtable(t(as.data.frame(boxreturns_pvalue)), digits = 3)

View(returns)

#### QUESTION 2 ####

#Import Data

yield <- scan(file="yield.txt")
yield <- ts(yield)
View(yield)
dyield <- diff(yield)
View(dyield)

#Part A
#ADF Test for Non-stationarity on Original Data
adfyield_pvalue<-c(1:20)
for (i in c(1:20)) {
  adfyield_pvalue[i]<-adf.test(yield,k=i)$p.value
}
adfyield_pvalue


#Box Test for Stationarity on Original Data
boxyield_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxyield_pvalue[i]<-Box.test(yield,lag=i,type="Ljung")$p.value
}
boxyield_pvalue

#Bartlett Test for ARIMA(0,1,0) Residuals
bartlett(Arima(yield, order=c(0,1,0))$residuals)

#ACF & PACF of Fitted Residuals
fit1 <- Arima(yield, order=c(0,1,0))
res1 <- residuals(fit1)
tsdisplay(res1)

#Box Test for White Noise in ARIMA(0,1,0)
boxdyield_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxdyield_pvalue[i]<-Box.test(Arima(yield, order=c(0,1,0))$residuals,lag=i,type="Ljung")$p.value
}
boxdyield_pvalue

##Part B

arima_p_1_p_aic<-c(1:3)
for (i in c(1:3)) {
  arima_p_1_p_aic[i]<-arima(yield, c(i, 1, i))$aic
}
arima_p_1_p_aic

#Part C

arima_p_1_q_aic<-data.frame(c(1:6),c(1:6),c(1:6),c(1:6),c(1:6),c(1:6))
for (i in c(0:5)) {
  for (j in c(0:5)) {
    arima_p_1_q_aic[i+1,j+1]<-arima(yield, c(i, 1, j))$aic
  }
}
arima_p_1_q_aic

#note also
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

#(iii)
sarima_p_1_q_aic<-data.frame(c(1:5),c(1:5),c(1:5),c(1:5),c(1:5),c(1:5))
for (i in c(0:4)) {
  for (j in c(0:5)) {
    sarima_p_1_q_aic[i+1,j+1]<-Arima(fatalities, order=c(i,1,j), seasonal=list(order = c(i, 1, j), period = 12))$aic
  }
}

for (i in c(3)) {
  for (j in c(0, 2, 3, 4,5)) {
    sarima_p_1_q_aic[i+1,j+1]<-Arima(fatalities, order=c(i,1,j), seasonal=list(order = c(i, 1, j), period = 12))$aic
  }
}
sarima_p_1_q_aic
xtable(sarima_p_1_q_aic, digits = 3)


Arima(fatalities, order=c(1,1,0), seasonal=list(order = c(1, 1, 0), period = 12))$aic
Arima(fatalities, order=c(0,1,1), seasonal=list(order = c(0, 1, 1), period = 12))$aic
Arima(fatalities, order=c(1,1,1), seasonal=list(order = c(1, 1, 1), period = 12))$aic
Arima(fatalities, order=c(0,1,2), seasonal=list(order = c(0, 1, 2), period = 12))$aic
Arima(fatalities, order=c(3,1,1), seasonal=list(order = c(3, 1, 1), period = 12))$aic
Arima(fatalities, order=c(4,1,1), seasonal=list(order = c(4, 1, 1), period = 12))$aic
Arima(fatalities, order=c(4,1,5), seasonal=list(order = c(4, 1, 5), period = 12))$aic
Arima(fatalities, order=c(5,1,1), seasonal=list(order = c(5, 1, 1), period = 12))$aic

#(iiii)
fit3 <- Arima(fatalities, order=c(0,1,1), seasonal=list(order = c(0, 1, 1), period = 12))
res <- residuals(fit3)
tsdisplay(res)
Box.test(res, lag=12, type="Ljung")
Box.test(res, lag=5, type="Ljung")
bartlett(res)


#Part 2
plot(forecast(fit3, h=12))

#Part 3

subfatalities<-fatalities[c(1:168)]
fit4 <- Arima(subfatalities, order=c(0,1,1), seasonal=list(order = c(0, 1, 1), period = 12))

plot(forecast(fit4, h=12))
lines(c(169:180), fatalities[c(169:180)], type="l", lty=1, col=c(2))
legend(0, 250, c("Predicted Values", "Realized Values"), lty=c(1,1), lwd=c(2.5,2.5), col=c(4,2),bty = "n")

#standard deviation in absolute difference between predicted values and realized
preddiff<-abs(as.data.frame(forecast(fit4, h=12))$`Point Forecast`-fatalities[c(169:180)])
sd(preddiff)

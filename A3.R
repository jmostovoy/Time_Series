### Preamble #############################
library(stats)
library(tseries)
library(forecast)
library(xtable)

### Question 1 #############################

#Import Data
setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)

# Part A =================================
adfdollar_pvalue<-c(1:20)
for (i in c(1:20)) {
  adfdollar_pvalue[i]<-adf.test(dollar,k=i)$p.value
}
adfdollar_pvalue

xtable(t(as.data.frame(adfdollar_pvalue)), digits = 3)

# Part B =================================

source("bartlett.txt")
bartlett(returns)
boxreturns_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxreturns_pvalue[i]<-Box.test(returns,
                                 lag=i,type="Ljung")$p.value
}
boxreturns_pvalue
xtable(t(as.data.frame(boxreturns_pvalue)), digits = 3)

View(returns)


### Question 2 ############################# 

#Import Data

yield <- scan(file="yield.txt")
yield <- ts(yield)
View(yield)
dyield <- diff(yield)
View(dyield)

# Part A =================================
#ADF Test for Non-stationarity on Original Data
adfyield_pvalue<-c(1:20)
for (i in c(1:20)) {
  adfyield_pvalue[i]<-adf.test(yield,k=i)$p.value
}
adfyield_pvalue
xtable(t(as.data.frame(adfyield_pvalue)), digits = 3)

#Box Test for Stationarity on Original Data
boxyield_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxyield_pvalue[i]<-Box.test(yield,
                               lag=i,type="Ljung")$p.value
}
boxyield_pvalue

#Bartlett Test for ARIMA(0,1,0) Residuals
bartlett(Arima(yield, order=c(0,1,0))$residuals)

#ACF & PACF of Fitted Residuals
fit1 <- Arima(yield, order=c(0,1,0))
ARIMA_0_1_0_Residuals <- residuals(fit1)
tsdisplay(ARIMA_0_1_0_Residuals)

#Box Test for White Noise in ARIMA(0,1,0)
boxdyield_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxdyield_pvalue[i]<-Box.test(Arima(yield, 
                          order=c(0,1,0))$residuals,
                          lag=i,type="Ljung")$p.value
}
boxdyield_pvalue
xtable(t(as.data.frame(10^6*boxdyield_pvalue)), digits = 3)

# Part B =================================
arima_p_1_p_aic<-c(1:3)
for (i in c(1:3)) {
  arima_p_1_p_aic[i]<-arima(yield, c(i, 1, i))$aic
}
arima_p_1_p_aic

# Part C =================================
arima_p_1_q_aic<-data.frame(c(1:6),c(1:6),c(1:6),c(1:6),c(1:6),c(1:6))
for (i in c(0:5)) {
  for (j in c(0:5)) {
    arima_p_1_q_aic[i+1,j+1]<-arima(yield, c(i, 1, j))$aic
  }
}
arima_p_1_q_aic
xtable(as.data.frame(arima_p_1_q_aic), digits = 3)

#note also
auto.arima(yield)

# Part D =================================
#Bartlett Tests
bartlett(arima(yield,c(3, 1, 3))$residuals)
bartlett(arima(yield,c(4, 1, 5))$residuals)

#Tsdiag
tsdiag(arima(yield,c(3, 1, 3)))
tsdisplay(residuals(arima(yield,c(3, 1, 3))))

tsdiag(arima(yield,c(4, 1, 5)))
tsdisplay(residuals(arima(yield,c(4, 1, 5))))

#Box Tests

boxdyield_pvalue_3_1_3<-c(1:15)
for (i in c(1:15)) {
  boxdyield_pvalue_3_1_3[i]<-Box.test(Arima(yield, 
                          order=c(3,1,3))$residuals,lag=i,type="Ljung")$p.value
}
boxdyield_pvalue_3_1_3
xtable(t(as.data.frame(boxdyield_pvalue_3_1_3)), digits = 3)

boxdyield_pvalue_4_1_5<-c(1:15)
for (i in c(1:15)) {
  boxdyield_pvalue_4_1_5[i]<-Box.test(Arima(yield, 
                    order=c(4,1,5))$residuals,lag=i,type="Ljung")$p.value
}
boxdyield_pvalue_4_1_5
xtable(t(as.data.frame(boxdyield_pvalue_4_1_5)), digits = 3)

#QQ Plot To Determine Normality or Not
qqnorm(Arima(yield, order=c(4,1,5))$residuals)
qqline(Arima(yield, order=c(4,1,5))$residuals)

qqnorm(Arima(yield, order=c(3,1,3))$residuals)
qqline(Arima(yield, order=c(3,1,3))$residuals)


### Question 3 ############################# 

fatalities <- scan(file="fatalities.txt")
lfatalities <- ts(log(fatalities))
View(lfatalities)

# Part A =================================
# (i) ---------------------------------
dlfatalities <- diff(diff(lfatalities,lag=12), lag=1)
View(dlfatalities)

auto.arima(diff(diff(lfatalities,lag=12)))

# (ii) ---------------------------------
acf(dlfatalities)
pacf(dlfatalities)

# (iii) ---------------------------------
sarima_p_1_q_aic<-data.frame(c(1:5),c(1:5),c(1:5),c(1:5),c(1:5),c(1:5))
for (i in c(0:4)) {
  for (j in c(0:5)) {
    sarima_p_1_q_aic[i+1,j+1]<-Arima(lfatalities, order=c(i,1,j),
                          seasonal=list(order = c(i, 1, j), period = 12))$aic
  }
}

for (i in c(3)) {
  for (j in c(0, 2, 3, 4,5)) {
    sarima_p_1_q_aic[i+1,j+1]<-Arima(lfatalities, order=c(i,1,j), 
                                     seasonal=list(order = c(i, 1, j), 
                                                   period = 12))$aic
  }
}
sarima_p_1_q_aic
xtable(sarima_p_1_q_aic, digits = 3)

# (iiii) ---------------------------------
fit3 <- Arima(lfatalities, order=c(0,1,1), 
              seasonal=list(order = c(0, 1, 1), period = 12))
res <- residuals(fit3)
tsdisplay(res)
bartlett(res)

boxres_pvalue<-c(1:15)
for (i in c(1:15)) {
  boxres_pvalue[i]<-Box.test(res,lag=i,type="Ljung")$p.value
}
boxres_pvalue
xtable(as.data.frame(t(boxres_pvalue)), digits=3)


# Part 2 =================================
plot(forecast(fit3, h=12))
fo<-forecast(fit3, h=12)
fo$upper[,c(2)]
fo$mean
fo$lower[,c(2)]
xtable(as.data.frame(t(as.data.frame((fo$mean), (fo$upper[,c(2)]), 
                                     fo$lower[,c(2)]))))
forecast(fit3, h=12)
# Part 3 =================================
subfatalities<-lfatalities[c(1:168)]
fit4 <- Arima(subfatalities, order=c(0,1,1), 
              seasonal=list(order = c(0, 1, 1), period = 12))

plot(forecast(fit4, h=12))
lines(c(169:180), lfatalities[c(169:180)], type="l", 
      lty=1, col=c(2))
legend(0, 5.6, c("Predicted Values", "Realized Values"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c(4,2),bty = "n")

#standard deviation in absolute difference between predicted values and realized
preddiff<-abs(as.data.frame(forecast(fit4, 
                      h=12))$`Point Forecast`-lfatalities[c(169:180)])
sd(preddiff)

# Table of Values

fo1<-forecast(fit4, h=12)
xtable(t(as.data.frame(fo1$lower[,c(2)])))
xtable(t(as.data.frame(fo1$mean)))
xtable(t(as.data.frame(lfatalities[c(169:180)])))
fo1$lower[,c(2)]


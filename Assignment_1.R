####Preamble####

library(stats)

####Question 1####

#Part a
setwd("~/Documents/Time_Series")
dollar <- scan(file="dollar.txt")
dollar <- ts(log(dollar))
View(dollar)
returns <- diff(dollar)
View(returns)
Box.test(returns, lag = 4, type = "Ljung-Box")

acf(dollar)
spec.pgram(dollar)

#Part b
acf(returns)
spec.pgram(returns)

#Specral Analysis of Raw Data for stocks
dollarspec<-spec.pgram(dollar, plot = F)
View(as.data.frame(dollarspec$freq))
View(as.data.frame(dollarspec$spec))

returnsspec<-spec.pgram(returns, plot = F)
View(as.data.frame(returnsspec$freq))
View(as.data.frame(returnsspec$spec))

#Part d
acf(abs(returns))
spec.pgram(abs(returns))

absreturnsspec<-spec.pgram(abs(returns), plot=F)
mean(returnsspec$spec)
mean(absreturnsspec$spec)

####Question 2####


#Part a
RCO2 <- scan(file="CO2.txt")
RCO2 <- as.numeric(RCO2)
RCO2 <-as.data.frame(RCO2)
RCO2 <-ts(RCO2)
View(RCO2)

spec.pgram(RCO2)

#Specral Analysis Raw Data for CO2
RCO2spec<-spec.pgram(RCO2, plot = F)
View(as.data.frame(RCO2spec$freq))
View(as.data.frame(RCO2spec$spec))

#Part b
RCO2_trend <- scan(file="CO2-trend.txt")
RCO2_trend <- as.numeric(RCO2_trend)
RCO2_trend <- as.data.frame(RCO2_trend)
RCO2_trend <- ts(RCO2_trend)
RCO2_sub_trend <- RCO2-RCO2_trend
View(RCO2_sub_trend)


spec.pgram(RCO2)

RCO2_sub_trendspec<-spec.pgram(RCO2_sub_trend, plot = F)
spec.pgram(RCO2_sub_trend, lty=1, col=plot_colours1[2], main = "")
lines(RCO2spec$freq, RCO2spec$spec, type="l", lty=1, col=plot_colours1[1])
title(main="Raw Periodograms for CO2 & CO2 Minus Trend Data", col.main="forestgreen", font.main=4)
legend(RCO2spec$freq[230], RCO2spec$spec[60], 
       c("CO2 Raw", "CO2 Minus Trend"), lty=c(1,1), lwd=c(2,2),cex=1.1, bty = "n", col=plot_colours1)


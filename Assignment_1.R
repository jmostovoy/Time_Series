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


acf(dollar)
spec.pgram(dollar)

#Part b
acf(returns)
spec.pgram(returns)

#Part d
acf(abs(returns))
spec.pgram(abs(returns))

####Question 2####


#Part a
RCO2 <- scan(file="CO2.txt")
RCO2 <- as.numeric(RCO2)
RCO2 <-as.data.frame(RCO2)
RCO2 <-ts(RCO2)
View(RCO2)

spec.pgram(RCO2)

#Part b
RCO2_trend <- scan(file="CO2-trend.txt")
RCO2_trend <- as.numeric(RCO2_trend)
RCO2_trend <- as.data.frame(RCO2_trend)
RCO2_trend <- ts(RCO2_trend)
RCO2_sub_trend <- RCO2-RCO2_trend
View(RCO2_sub_trend)

spec.pgram(RCO2_sub_trend)


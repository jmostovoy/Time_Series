####Preamble####

library(stats)

####Question 2####

#Part a
setwd("~/Documents/Time_Series")
flour <- scan(file="flour.txt")
flour <- ts(flour)
View(flour)

plot(flour,  type="l", col=4,lty=1, ann=FALSE)
title(xlab="Month", col.lab=rgb(0,0.6,.7))
title(ylab="Price Index" , col.lab=rgb(0,0.6,.7))
title(main="Time Series Plot of Flour Data", 
      col.main="forestgreen", font.main=4)

#Part b

acf(flour)
spec.pgram(flour)
pacf(flour)

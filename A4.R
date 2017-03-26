#A4
### Preamble #############################
library(stats)
library(tseries)
library(forecast)
library(xtable)
library(fGarch)
library(FinTS)
source("bartlett.txt")
source("spec.parzen.txt")


### Question 1 ###
fatal <- scan("fatalities.txt")
fatal <- ts(fatal, start=c(1960,1),end=c(1974,12),freq=12)

# want to pick the param on stl to minimize the remainder (i.e. explain most)
# as such we play around with the parametres to see which combination is best

# begin with extreme cases

# seasonal smoothing with only 1 month and trend with 11 months 
r1 <- stl(fatal, s.window = 1, t.window = 11, robust = T)
plot(r1, main = "R1")
# seasonal smoothing with "periodic" (i.e. 11 months) and trend with 167 months
r2 <- stl(fatal, s.window = 11, t.window = 167)
plot(r2, main = "R2")
r2$time.series[,c(1)]

# try to optimize with logical choices of parametres 

# seasonal smoothing with only 3 month (spring, summer, fall, winter as the number of 
# crashes in each of the seasons is expected to vary) and trend with 11 months
r3 <- stl(fatal, s.window = 3, t.window = 11)
plot(r3, main = "R3")

# seasonal smoothing with semi-annual (wet vs. dry season) and trend with 11 months (1 year)
r4 <- stl(fatal, s.window = 6, t.window = 11)
plot(r4, main = "R4")


# seasonal smoothing with semi-annual (wet vs. dry season) and trend with 23 months (2 year))
r5 <- stl(fatal, s.window = 3, t.window = 23)
plot(r5, main = "R5")
# seasonal smoothing with semi-annual and trend with 23 months (2 year)
r6 <- stl(fatal, s.window = 6, t.window = 23)
plot(r6, main = "R6")


# seasonal smoothing with semi-annual (wet vs. dry season) and trend with 11 months (1 year)
r4 <- stl(fatal, s.window = 6, t.window = 11)
plot(r4, main = "R4")

# seasonal smoothing with 3 months and trend with 47 months (4 years)
r_opt <- stl(fatal, s.window = 3, t.window = 47)
plot(r_opt, main = "R_Opt")

### Question 1 #############################

#Import Data
setwd("~/Documents/Time_Series")
fatal <- scan(file="fatalities.txt")
fatal <- ts(log(fatal), start=c(1960,1),end=c(1974,12),freq=12)
View(fatal)

# Part A =================================

stls <- list()
for(i in seq(1, 99, 2)){
a <- stl(fatal,s.window = 1, t.window = i)
b <- stl(fatal,s.window = 3, t.window = i)
c <- stl(fatal,s.window = 5, t.window = i)
d <- stl(fatal,s.window = 7, t.window = i)
e <- stl(fatal,s.window = 9, t.window = i)
f <- stl(fatal,s.window = 11, t.window = i)
name <- paste('t',i,sep='')
tmp <- list(s1=a, s3=b, s5=c, s7=d, s9=e, s11=f)
stls[[name]] <- tmp
}

for (i in c(1:50)){
  for (j in c(1:6)) {
    plot(stls[[i]][[j]])
    title(outer=paste("stl for ", "t",2*i-1 ,"s", 2*j-1, sep = ""), col.main="forestgreen", font.main=3)
  }
}

plot(stls[[1]][[1]])
title(main=paste("stl for ", "t",2*1-1 ,"s", 2*1-1, sep = ""), col.main="forestgreen", font.main=3)

get("stls$t1$s1")$get(paste("t",1,sep=""))$s1


# Part B =================================
# Part C =================================

### Question 2 #############################
speech <- ts(scan(file="speech.txt"),frequency=10000)
View(speech)

# Part A =================================
rr<-c(1:200)

rr <- list(rep(list(freq=c(1:1025), spec=c(1:1025), M=c(1), std.err=c(1)),200))
for(i in c(1:200)) {
  rr[[i]]<-spec.parzen(speech,maxlag=i,plot=T)
}


r<-spec.parzen(speech,maxlag=i,plot=T)r
r$std.err
plot(r)

# Part B =================================
r <- spec.ar(speech,order=10,method="burg")
# Part C =================================


### Question 3 #############################
gold<-ts(scan(file="barrick.txt"))

# Part A =================================

#ADF Test
adf.test(gold)
adf.test(diff(gold))

#Fit Models
gold_011<-Arima(gold,order=c(0,1,1))
gold_012<-Arima(gold,order=c(0,1,2))

#AIC for each
gold_011$aic
gold_012$aic

#TS Display
tsdisplay(gold_011$residuals)
tsdisplay(gold_012$residuals)

#TS Diagram
tsdiag(gold_011)
tsdiag(gold_012)

#Bartlett
bartlett(gold_011$residuals)
bartlett(gold_012$residuals)

#Auto-ARIMA
auto.arima(gold)

# Part B =================================
#Res of ARIMA(0,1,2)
resgold_011<-Arima(gold,order=c(0,1,1))$residuals
resgold_012<-Arima(gold,order=c(0,1,2))$residuals
#

garchtests<-c(1:10)
for (i in c(1:5)) {
  garchtests[i]<- garchFit(data=resgold_012, formula = ~ garch(i, 0))$p.value
}
for (i in c(6:10)) {
  garchtests[i]<-garchFit(data=resgold_012, formula = ~ garch(i-5, 0))$statistic
}
garchtests


garchFit(~ garch(i, 0),data=resgold_012)$p.value
ARCH2 <- garchFit(~ garch(1,0), data = resgold_012, trace = FALSE) 
summary(ARCH2)

# Part C =================================
garch(gold, order=c(1,1))
garch(gold, order=c(1,2))
garch(gold, order=c(1,3))
garch(gold, order=c(1,4))
garch(gold, order=c(1,5))

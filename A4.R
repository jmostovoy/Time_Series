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
library(ggplot2)

### Question 1 #############################

#Import Data
setwd("~/Documents/Time_Series/plotsA4")
fatal <- scan(file="fatalities.txt")
fatal <- ts(log(fatal), start=c(1960,1),end=c(1974,12),freq=12)
View(fatal)

# Part A =================================

stls <- list()
for(i in seq(1, 167, 2)){
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

for (i in c(9:50)){
  for (j in c(1:6)) {
    pdf(paste("stl_", "t_",2*i-1, "_s_", 2*j-1,".pdf", sep = ""))
    plot(stls[[i]][[j]],main=paste("stl for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
    dev.off()
  }
}


# Part B =================================

for (i in c(9:50)){
  for (j in c(1:6)) {
    acf(stls[[i]][[j]]$time.series[,3],
        main=paste("acf of stl's irregular component for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
  }
}

pacf(as.datafra(stls[[13]][[3]]$time.series[,3]))
stls[[1]][[1]]$time.series

# Part C =================================



### Question 2 #############################
speech <- ts(scan(file="speech.txt"),frequency=10000)
View(speech)

# Part A =================================
sp <- list()
for(i in c(1:200)){
  name <- paste(i,sep='')
  pdf(paste("sp", "_",i,".pdf", sep = ""))
  tmp <- spec.parzen(speech,maxlag=i,plot=T)
  dev.off()
  sp[[name]] <- tmp
}



r<-spec.parzen(speech,maxlag=i,plot=T)r
r$std.err
plot(r)

# Part B =================================
sa <- list()
for(i in c(1:200)){
  name <- paste(i,sep='')
  pdf(paste("sa", "_",i,".pdf", sep = ""))
  tmp <- spec.ar(speech,order=i,method="burg")
  dev.off()
  sa[[name]] <- tmp
}

pdf(paste("sa18",".pdf", sep = ""))
r <- spec.ar(speech,method="yule-walker")
dev.off()
r$freq[which.max(r$spec)]
r$freq[64]
# Part C =================================
#See write up

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

ARCH1<-garchFit(data=resgold_012, formula = ~ garch(1, 0), trace=F)
ARCH2<-garchFit(data=resgold_012, formula = ~ garch(2, 0), trace=F)
ARCH3<-garchFit(data=resgold_012, formula = ~ garch(3, 0), trace=F)
ARCH4<-garchFit(data=resgold_012, formula = ~ garch(4, 0), trace=F)
ARCH5<-garchFit(data=resgold_012, formula = ~ garch(5, 0), trace=F)

summary(ARCH3)

# Part C =================================
GARCH11<-garchFit(data=resgold_012, formula = ~ garch(1, 1), trace=F)
GARCH21<-garchFit(data=resgold_012, formula = ~ garch(2, 1), trace=F)
GARCH31<-garchFit(data=resgold_012, formula = ~ garch(3, 1), trace=F)
GARCH41<-garchFit(data=resgold_012, formula = ~ garch(4, 1), trace=F)
GARCH51<-garchFit(data=resgold_012, formula = ~ garch(5, 1), trace=F)

summary(GARCH31)

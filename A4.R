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
setwd("~/Documents/Time_Series")
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
length(stls)

setwd("~/Documents/Time_Series/A4_STL_Plots")
for (i in c(9:84)){
  for (j in c(1:6)) {
    pdf(paste("stl_", "t_",2*i-1, "_s_", 2*j-1,".pdf", sep = ""))
    plot(stls[[i]][[j]],main=paste("stl for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
    dev.off()
  }
}

#robust

stlsr <- list()
for(i in seq(1, 167, 2)){
  a <- stl(fatal,s.window = 1, t.window = i, robust=T)
  b <- stl(fatal,s.window = 3, t.window = i, robust=T)
  c <- stl(fatal,s.window = 5, t.window = i, robust=T)
  d <- stl(fatal,s.window = 7, t.window = i, robust=T)
  e <- stl(fatal,s.window = 9, t.window = i, robust=T)
  f <- stl(fatal,s.window = 11, t.window = i, robust=T)
  name <- paste('t',i,sep='')
  tmp <- list(s1=a, s3=b, s5=c, s7=d, s9=e, s11=f)
  stlsr[[name]] <- tmp
}
length(stlsr)

setwd("~/Documents/Time_Series/A4_STL_Robust_Plots")
for (i in c(9:84)){
  for (j in c(1:6)) {
    pdf(paste("stl_robust_", "t_",2*i-1, "_s_", 2*j-1,".pdf", sep = ""))
    plot(stlsr[[i]][[j]],main=paste("stl robust for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
    dev.off()
  }
}


# Part B =================================
#All ACFs
setwd("~/Documents/Time_Series/A4_STL_ACFs")
for (i in c(9:84)){
  for (j in c(1:6)) {
    pdf(paste("stl_acf_", "t_",2*i-1, "_s_", 2*j-1,".pdf", sep = ""))
    acf(stls[[i]][[j]]$time.series[,3],
        main=paste("acf of stl's irregular component for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
    dev.off()
  }
}

#All PACFs
setwd("~/Documents/Time_Series/A4_STL_PACFs")
for (i in c(9:84)){
  for (j in c(1:6)) {
    pdf(paste("stl_pacf_", "t_",2*i-1, "_s_", 2*j-1,".pdf", sep = ""))
    pacf(stls[[i]][[j]]$time.series[,3],
        main=paste("pacf of stl's irregular component for ", "t=",2*i-1, " " ,"and s=", 2*j-1, sep = ""))
    dev.off()
  }
}

#Bartlett for t=83, s=11
bartlett(stls[[42]][[6]]$time.series[,3])




#Box Test
box_test_pvalue<-c(1:15)
for (i in c(1:15)) {
  box_test_pvalue[i]<-Box.test(stls[[42]][[6]]$time.series[,3], lag=i,type="Ljung")$p.value
}
box_test_pvalue
xtable(t(as.data.frame(box_test_pvalue)), digits = 4)


# Part C =================================



### Question 2 #############################
speech <- ts(scan(file="speech.txt"),frequency=10000)
View(speech)

# Part A =================================
setwd("~/Documents/Time_Series/A4_SP_Plots")
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

setwd("~/Documents/Time_Series/A4_SA_Plots")
sa <- list()
for(i in c(1:200)){
  name <- paste(i,sep='')
  pdf(paste("sa", "_",i,".pdf", sep = ""))
  tmp <- spec.ar(speech,order=i,method="burg")
  dev.off()
  sa[[name]] <- tmp
}

pdf(paste("sa_YW",".pdf", sep = ""))
r <- spec.ar(speech,method="yule-walker")
dev.off()
r$freq[which.max(r$spec)]
r$freq[64]
# Part C =================================
#See write up and:
r$freq[which.max(r$spec)]


spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=20,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=40,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=60,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=80,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=100,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=120,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=140,method="burg")$spec)]
spec.ar(speech,order=i,method="burg")$freq[which.max(spec.ar(speech, order=160,method="burg")$spec)]
#hi


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

summary(ARCH1)
summary(ARCH2)
summary(ARCH3)
summary(ARCH4)
summary(ARCH5)

# Part C =================================
GARCH11<-garchFit(data=resgold_012, formula = ~ garch(1, 1), trace=F)
GARCH21<-garchFit(data=resgold_012, formula = ~ garch(2, 1), trace=F)
GARCH31<-garchFit(data=resgold_012, formula = ~ garch(3, 1), trace=F)
GARCH41<-garchFit(data=resgold_012, formula = ~ garch(4, 1), trace=F)
GARCH51<-garchFit(data=resgold_012, formula = ~ garch(5, 1), trace=F)

summary(GARCH11)
summary(GARCH21)
summary(GARCH31)
summary(GARCH41)
summary(GARCH51)

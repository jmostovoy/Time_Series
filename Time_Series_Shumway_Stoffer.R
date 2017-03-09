#### Code to Assist in Leaning ####

install.packages("astsa")
library(astsa)

####Chapter 1####
#pg 2
plot(jj, type="o", ylab="Quarterly Earnings per Share") 
plot(log(jj)) # not shown

#pg 3
plot(gtemp, type="o", ylab="Global Temperature Deviations")
plot(nyse, ylab="NYSE Returns")

#pg 4
par(mfrow = c(2,1)) # set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index") 
plot(rec, ylab="", xlab="", main="Recruitment")

#pg 5
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalam & Cereb")
mtext("Time (1 pt = 2 sec)", side=1, line=2)

#pg 6

w = rnorm(500,0,1) # 500 N(0,1) variates 
v = filter(w, sides=2, rep(1/3,3)) # moving average par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

#pg 10

cs = 2*cos((2*pi*1:500)/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))


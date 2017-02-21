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





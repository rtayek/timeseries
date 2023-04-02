library(astsa) #SEE THE FOOTNOTE
plot(jj, type="o", ylab="Quarterly Earnings per Share") 
plot(globtemp, type="o", ylab="Global Temperature Deviations") 
plot(speech)
library(xts)
library(forecast)
nd=ndiffs(djia)
print(nd)
djiar = diff(log(djia$Close))[-1] #approximate returns
nd=ndiffs(djiar)
print(nd)
plot(djiar, main="DJIA Returns", type="n")
lines(djiar) 
par(mfrow = c(2,1)) #set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment") 
par(mfrow=c(2,1))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", main="Thalamus & Cerebellum")
par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion") 
w = rnorm(500,0,1) #500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3,3)) #moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average") 
v = rnorm(550,0,1) #50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)] #remove first 50
plot.ts(x, main="autoregression")
set.seed(154) #so you can reproduce the results
w = rnorm(200); x = cumsum(w) #two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2) 

cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

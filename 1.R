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
# 1.5 stationarity
x = rnorm(100)
y = lag(x, -5) + rnorm(100)
ccf(y, x, ylab='CCovF', type='covariance') 

# Example 1.25 Sample ACF and Scatterplots
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) #first 6 sample acf values  [1] 0.604 0.374 0.214 0.050 -0.107 -0.187
par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])

# Example 1.26 A Simulated Time Series
set.seed(101010)
x1 = 2*rbinom(11, 1, .5) - 1 #simulated sequence of coin tosses
x2 = 2*rbinom(101, 1, .5) - 1
y1 = 5 + filter(x1, sides=1, filter=c(1,-.7))[-1]
y2 = 5 + filter(x2, sides=1, filter=c(1,-.7))[-1]
plot.ts(y1, type='s'); plot.ts(y2, type='s') #plot both series (not shown)
c(mean(y1), mean(y2)) #the sample means  [1] 5.080 5.002
acf(y1, lag.max=4, plot=FALSE) #1/ √ 10 = .32  Autocorrelations of series 'y1', by lag  01234  1.000 -0.688 0.425 -0.306 -0.007 
acf(y2, lag.max=4, plot=FALSE) #1/ √ 100 = .1  Autocorrelations of series 'y2', by lag  01234  1.000 -0.480 -0.002 -0.004 0.000
# Note that the sample ACF at lag zero is always 1 (Why?).

# Example 1.27 ACF of a Speech Signal
acf(speech,250)

# Example 1.28 SOI and Recruitment Correlation Analysis
par(mfrow=c(3,1))
acf(soi, 48, main="Southern Oscillation Index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="SOI vs Recruitment", ylab="CCF")

# Example 1.29 Prewhitening and Cross Correlation Analysis
set.seed(1492)
num=120; t=1:num 
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL)) 
par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1)) 

plot(X)
plot(Y)
acf(X,48, ylab='ACF(X)')
acf(Y,48, ylab='ACF(Y)')
ccf(X,Y,24, ylab='CCF(X,Y)')
ccf(X,Yw,24, ylab='CCF(X,Yw)', ylim=c(-.6,.6)) 

# Example 1.30 Soil Surface Temperatures  As an example, the two-dimensional (r = 2)
persp(1:64, 1:36, soiltemp, phi=25, theta=25, scale=FALSE, expand=4,  ticktype="detailed", xlab="rows", ylab="cols", zlab="temperature")
plot.ts(rowMeans(soiltemp), xlab="row", ylab="Average Temperature") 

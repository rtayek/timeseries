# Chapter 2
# Time Series Regression and Exploratory  Data Analysis 
library(astsa) #SEE THE FOOTNOTE
library(xts)
library(forecast)
summary(fit <- lm(chicken~time(chicken), na.action=NULL))
# Estimate Std.Error t.value
# (Intercept) -7131.02 162.41 -43.9
# time(chicken) 3.59 0.08 44.4
# --  
# Residual standard error: 4.7 on 178 degrees of freedom
plot(chicken, ylab="cents per pound")
abline(fit) #add the fitted line  The multiple linear regression model described by (2.1)
str(chicken)

# Definition 2.3 Bayesian Information Criterion (BIC) 
par(mfrow=c(3,1)) #plot the data
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")
dev.new() #open a new graphic device 
ts.plot(cmort,tempr,part, col=1:3) #all on same plot (not shown) 
dev.new() 
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
temp = tempr-mean(tempr) #center temperature
temp2 = temp^2
trend = time(cmort) #time
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit) #regression results
summary(aov(fit)) #ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) #Table 2.1 
num = length(cmort) #sample size
AIC(fit)/num - log(2*pi) #AIC
BIC(fit)/num - log(2*pi) #BIC
(AICc = log(sum(resid(fit)^2)/num)+(num+5)/(num-5-2)) #AICc  As previously mentioned, it is possible to include lagged variables in

# Example 2.3 Regression With Lagged Variables
dev.off()
dev.new() 
fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(fit1 <- lm(rec~soiL6, data=fish, na.action=NULL))

# The headache of aligning the lagged series can be avoided by using the R package  dynlm, which must be downloaded and installed.
#library(dynlm) # not available for this version
#summary(fit2 <- dynlm(rec~ L(soi,6))) 

# Example 2.6 Differencing Global Temperature  The global temperature series shown in Fig. 1.2
plot(globtemp, type="o", ylab="Global Temperature Deviations") 

par(mfrow=c(2,1))
plot(diff(globtemp), type="o")
mean(diff(globtemp)) #drift estimate = .008
acf(diff(gtemp), 48) 


# Example 2.7 Paleoclimatic Glacial Varves
par(mfrow=c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="")

# Example 2.8 Scatterplot Matrices, SOI and Recruitment
# Southern Oscillation  Index (SOI) 
# Recruitment (number of new fish)
lag1.plot(soi, 12) #Fig. 2.8 
lag2.plot(soi, rec, 8) #Fig. 2.9 

dummy = ifelse(soi<0, 0, 1)
fish = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)  summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))
# Coefficients:  Estimate Std.Error t.value
# (Intercept) 74.479 2.865 25.998 
# soiL6 -15.358 7.401 -2.075  dL6 -1.139 3.711 -0.307
# soiL6:dL6 -51.244 9.523 -5.381
# ---  Residual standard error: 21.84 on 443 degrees of freedom
# Multiple R-squared: 0.4024  F-statistic: 99.43 on 3 and 443 DF
attach(fish)
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
plot(resid(fit)) #not shown... 
acf(resid(fit)) #... but obviously not noise 

# Example 2.10 Using Regression to Discover a Signal in Noise
set.seed(90210) #so you can reproduce these results
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x~0+z1+z2)) #zero to exclude the intercept
# Coefficients:  Estimate Std. Error t value
# z1 -0.7442 0.3274 -2.273  z2 -1.9949 0.3274 -6.093
# Residual standard error: 5.177 on 498 degrees of freedom
par(mfrow=c(2,1))
plot.ts(x)
plot.ts(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2) 

# 2.3 Smoothing in the Time Series Context

# Example 2.11 Moving Average Smoother
# For example, Fig. 2.12 shows the monthly SOI series discussed in Example 1.5  smoothed using (2.37) with weights a0 = a±1 = ··· = a±5 = 1/12, and a±6 = 1/24; k = 6. 
# This particular method removes (filters out) the obvious annual temperature  cycle and helps emphasize the El Niño cycle. 
# To reproduce Fig. 2.12 in R:
#par(mar = c(1, 1, 1, 1)) # added by me
wgts = c(.5, rep(1,11), .5)/12
soif = filter(soi, sides=2, filter=wgts)
plot(soi)
lines(soif, lwd=2, col=4)
par(fig = c(.65, 1, .65, 1), new = TRUE) #the insert
nwgts = c(rep(0,20), wgts, rep(0,20))
plot(nwgts, type="l", ylim = c(-.02,.1), xaxt='n', yaxt='n', ann=FALSE)
# gets: Error in plot.new() : figure margins too large

# 2.3 Smoothing in the Time Series Context 
# Example 2.12 Kernel Smoothing  Kernel smoothing is a moving average
plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth=1), lwd=2, col=4)
par(fig = c(.65, 1, .65, 1), new = TRUE) #the insert 
gauss = function(x) {1/sqrt(2*pi) * exp(-(x^2)/2)}
x = seq(from = -3, to = 3, by = 0.001)
plot(x, gauss(x), type ="l", ylim=c(-.02,.45), xaxt='n', yaxt='n', ann=FALSE) 

# Example 2.13 Lowess
plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4) #El Nino cycle 
lines(lowess(soi), lty=2, lwd=2, col=2) #trend (with default span) 

plot(soi)
lines(smooth.spline(time(soi), soi, spar=.5), lwd=2, col=4) 
lines(smooth.spline(time(soi), soi, spar= 1), lty=2, lwd=2, col=2) 


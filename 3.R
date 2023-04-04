#chapter 3 ARIMA Models 
# what is small gamma
# Fugure 3.1
par(mar = c(.1, .1, .1, .1))
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",  main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",  main=(expression(AR(1)~~~phi==-.9))) 

# into to ma models

set.seed(8675309) #Jenny, I got your number
x = rnorm(150, mean=5) #generate iid N(5,1)s
arima(x, order=c(1,0,1)) #estimation

ARMAtoMA(ar = .9, ma = .5, 10) #first 10 psi-weights 
# [1] 1.40 1.26 1.13 1.02 0.92 0.83 0.74 0.67 0.60 0.54  The invertible representation using Property 3.1 is obtained

ARMAtoMA(ar = -.5, ma = -.9, 10) #first 10 pi-weights
# [1] -1.400 .700 -.350 .175 -.087 .044 -.022 .011 -.006 .003 

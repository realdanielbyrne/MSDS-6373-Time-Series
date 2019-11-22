library(tswge)
library(tseries)
swdelay1 = read.csv('swadelay.csv', header = TRUE )

# calculate variance of time stationary time series
# don't trust
myts.var = function(ts) {
  x = ts[!is.na(as.numeric(ts))]
  n=length(x) #n = 1509
  nlag=n-1 #n-1
  m=mean(x)
  v=var(x,na.rm = TRUE)
  gamma0 = var(x)*(n-1)/n
  aut=acf(x,lag.max=nlag) #n-1
  sum=0
  for  (k in 1:nlag) {sum=sum+(1-k/n)*aut$acf[k+1]*gamma0}
  vxbar=2*sum/n+gamma0/n   #note the mult of sum by 2 
  
  
  return (vxbar)
}

# calculate gamma[lag] given ts abnd lag
ts.gamma = function(ts,lag) {
  u = mean(ts) 
  n = length(ts)
  val = 0
  for (k in 1:(length(ts) - lag)) {
    val = val + (ts[k] - u) * ( ts[k + lag] - u)
  }
  return (val/n)
}

# calculate rho[lag] given ts and lag
ts.rho = function(ts,lag) {
  u = mean(ts)
  n = length(ts)
  val = 0
  gamma0 = ts.gamma(ts,0)
  for (k in 1:(length(ts) - lag)) {
    val = val + (ts[k] - u) * ( ts[k + lag] - u)
  }
  return ((val/n)/gamma0)
}

ase = function(f,s) {
  l = length(f) - 1
  ls = length(s)
  ase = mean((f - s[(ls - l):ls])^2)
  return (ase)
}



# generate a signal with signal plus noise
# coef = coefficients to cosine function
# freq =  (cos argument (not including 2pi))
# psi = phase offset
s = gen.sigplusnoise.wge(100,coef=c(3,0),freq = c(.083,0), plot = TRUE)

# Forecasting AR(p)
# Given Xt(0) predict Xt(k).  Work iteratively 1 right after another.

#AR(1) Phi Positive Forecasting
# exponentional damping forecast
data(fig6.1nf)
plotts.wge(fig6.1nf)
fore.arma.wge(fig6.1nf,phi=.8,n.ahead=20,plot=TRUE)
plotts.sample.wge(fig6.1nf)

# AR(1) Phi Negative Forecasting
# oscilating forecast
x1 = gen.arma.wge(100,phi=-.95)
plotts.sample.wge(x1)
fore.arma.wge(x1,phi=-.8,n.ahead=10,plot=TRUE,limits=FALSE)

# AR(2) Forecasting
# sinusoidal predictions
x2 = gen.arma.wge(n=75,phi=c(1.6,-.8),sn=24)
x2 = x2 + 25 # Add mean
factor.wge(c(1.6,-.8))
fore.arma.wge(x2,phi=c(1.6,-.8),n.ahead=80,limits=TRUE)

# Forecasting AR(2,1) 
x21 = gen.arma.wge(n=75,phi=c(1.6,-.8),theta = -.9,sn=24)
fore.arma.wge(x2,phi=c(1.6,-.8),theta = -.9,n.ahead=40,limits=FALSE)
# Forecasting AR(2,1) but with a AR(1) 
fore.arma.wge(x2,phi=.8,theta = -.9,n.ahead=40,limits=FALSE) #forecasting with AR1 from AR2 model

# Forecast llynx
data("llynx")
plotts.wge(llynx)
# forcasting with AR(4)
fore.arma.wge(llynx,phi=c(1.3,-.7,.1,-.2),n.ahead=40,limits=FALSE)
# forecasting with AR(4,0,1)
fore.arma.wge(llynx,phi=c(1.3,-.7,.1,-.2),theta = -.6,n.ahead=40,limits=FALSE)

# forecasting with ARIMA(0,1,0)
# straight line
fore.aruma.wge(llynx,d=1,n.ahead=40,limits=FALSE, plot = TRUE)

# forecasting with ARIMA(0,1,0) with s = 1
# diagonal line
fore.aruma.wge(llynx,d=1,n.ahead=40,s = 1,limits=FALSE, plot = TRUE)

# AR(2) behavior 2 positive, exp damping, wandering
x = gen.arma.wge(200, phi = c(1.4,-.48))
factor.wge(c(1.4,-.48))
plotts.sample.wge(x)

# AR(2) behavior 1 positive 1 negative two peaks in frequency spectrum
x = gen.arma.wge(200, phi = c(.2,.48))
factor.wge(c(.2,.48))
plotts.sample.wge(x)

# AR(2) behavior 2 negative, heavy osc
x = gen.arma.wge(200, phi = c(-1.4,-.48))
factor.wge(c(-1.4,-.48))
plotts.sample.wge(x)

# AR(2) complex conjugate roots
x = gen.arma.wge(200, phi = c(1.6,-.8))
factor.wge(c(1.6,-.8))
plotts.sample.wge(x)

# factor characteristic equation
# 1 -.2Z -.48Z^2
factor.wge(c(.2,.48))

# determine ar2 stationarity
# 1 - .2Z -.48Z^2
# 1 - 1.6Z + .15Z^2 not stationary because 1 root is inside unit circle,  abs(r) < 1
# 1 - 1.6Z + .8Z^2
factor.wge(c(-.2,.48))
factor.wge(c(1.6,-.15))
factor.wge(c(1.6,-.8))


# arima
arima000 = gen.arima.wge(300,c(0,0,0), d = 12)
acf(arima000)

#ar4 
# multiple humps in spectral density
x4 = gen.arma.wge(500,phi=c(-.2,.1,.3,.1))
plotts.sample.wge(x4)

#arima 1,2,1
#fast decaying exponential withg bumps in spectral density
arima121 = gen.arima.wge(100,phi=-.2,d=2,theta=.1)
plotts.sample.wge(arima121)

#signal plus noise
# generate a signal with signal plus noise
# coef = coefficients to cosine function
# freq =  (cos argument (not including 2pi))
# psi = phase offset
s=gen.sigplusnoise.wge(100,coef=c(3,1.5),freq = c(.05,.35), psi = c(0,2),vara = 3, plot = FALSE)
plotts.sample.wge(s)


# sunspot data
data(sunspot.year)
ssaic5= aic5.wge(sunspot.year)

fore.aruma.wge(sunspot.year,phi=c(.723,.283,-.519),theta=-.6,n.ahead=20)

fore.aruma.wge(sunspot.year,d=1,s=12,n.ahead=20)

f10ar2 = fore.aruma.wge(sunspot.year,phi=c(1.06,-.4),s=10,n.ahead=20, lastn = TRUE, limits= FALSE)
s = gen.aruma.wge(1000,phi=c(1.06,-.4),s=10)
plotts.sample.wge(x)
plotts.sample.wge(sunspot.year)
ase(f10ar2$f,s)


# forecast by hand
# (1 - .6B + .4B^2)Xt = at 
d = c(6,8,13,12,10,7,4,2,1)
xbar = mean(d)
phi = c(.6,-.4)
xhat9 = d[9]*phi[1] + d[8]*phi[2] + xbar*(1-phi[1] - phi[2])
xhat9

# forecast ar1 or ar2 model
ts.fore = function(ts,phi)  {
  n = length(ts)
  xbar = mean(ts)
  fore = ts[n]*phi[1] + ts[n-1]*phi[2] + xbar*(1-phi[1] - phi[2])
  return (fore)
}
ts.fore (d,phi)

# factor
factor.wge(phi=c(1.59,-.544,-.511,.222))
plotts.true.wge(phi=c(1.59,-.544,-.511,.222))

# forecast signal plus noise
x = gen.sigplusnoise.wge(n=50,b0=10,b1=.2,phi=c(.8,-.6))
xfore = fore.sigplusnoise.wge(x,linear = TRUE,n.ahead=20,lastn=FALSE,limits=F, plot = TRUE)

# calculatge psi weights
psi.weights.wge(phi=c(1.2,-.6),theta=.5,lag.max=5)

#airline model
airline = gen.aruma.wge(n=100,s=12,d=1,phi=c(.8,-.6))
plotts.sample.wge(airline)

# calculate forecast limits
# 1.96 * sqrt(wnv) * sqrt(1) for t1
# 1.96 * sqrt(wnv) * sqrt(1 + phi[1]^2) for t2

# Example x(t)(1-.6B+.4B^2) = at
d = c(5,9,13,15,14,10,12,17,20,25)

#gamma0
ts.gamma(d,0)

#p3
ts.rho(d,3)

# glp model
# Xt = at +.6at-1 - .04at-2 - .264at-3
psi = psi.weights.wge(phi=c(.6,-.4),lag.max=3)


# moe
f = fore.arma.wge(d,phi=c(.6,-.4))

moe = function(wnv,psi) {
  t01 = 1.96*sqrt(wnv)*sqrt(1)
  t02 = 1.96*sqrt(wnv)*sqrt(1 + phi[1]^2)
  return (c(t01,t02))
}

moe(f$wnv,psi)

#ma(2) with complex conjugate roots
x = gen.arma.wge(100,theta=c(1.6,-.9))
plotts.sample.wge(x)
factor.wge(c(1.6,-.8))

# find stationarity
# split realization and look for changing mean
# split realization and look for changing variance
# split and check for differing covariances


phi = c(1.65,-1.06,.262)

xbar = mean(d)
xhat11 = d[10]*phi[1] + d[9]*phi[2] + d[8]*phi[3] + xbar*(1-phi[1]-phi[2]-phi[3])
xhat11
xhat12 = xhat11*phi[1] + d[10]*phi[2] + d[9]*phi[3] + xbar*(1-phi[1]-phi[2]-phi[3])
xhat12
prob5 = fore.arma.wge(d,phi=phi,n.ahead=2)
prob5$f


psi = psi.weights.wge(phi=phi,lag.max = 2)
moe(prob5$wnv,psi)
halfs = moe(prob5$wnv,psi)
xhat12lower = xhat12 - halfs[2]
xhat12lower
xhat12upper = xhat12 + halfs[2]
xhat12upper

fore.arma.wge(d,phi=phi,n.ahead = 4)
factor.wge(phi = phi)

dominant = gen.arma.wge(100,phi=c(1.0256,-.4196))
plotts.sample.wge(dominant)

s = gen.arma.wge(100,phi=phi,plot=T)


s = gen.aruma.wge(100,d = 1,plot=T)
plotts.sample.wge(s)

par(mfrow=c(1, 2))
#acf(electricity$x[1:250])
#acf(electricity$x[251: 500])

# Take HOME 2
# Model 1, (1 - 2.0401B + 1.2159B^2 - .1751B^3)(1-B^336)Xt=〖(1-.9551B)at

#m11day = fore.aruma.wge(electricity$x, phi = c(2.0401,-1.2159, .1751), theta =.9551, s = 336, n.ahead = 4,  lastn = T, limits = T, plot = T)
#m17day = fore.aruma.wge(electricity$x, phi = c(2.0401,-1.2159, .1751), theta =.9551, s = 336, n.ahead = 10, lastn = T, limits = T, plot = T)
#ase(m17day$f,electricity$x)
#acf(m17day$f)


# Model 2, (1 -.0866B +.8452B^2 +.1804B^3)(1-B^336)(1-B)Xt=(1+.00026B -.926B^2)at
#m21day = fore.aruma.wge(electricity$x, phi = c(.0866, -.8452, -.1804), theta =c(-.00026,.926), s = 336, d=1, n.ahead = 4,  lastn = T, limits = T, plot = T)
#m27day = fore.aruma.wge(electricity$x, phi = c(.0866, -.8452, -.1804), theta =c(-.00026,.926), s = 336, d=1, n.ahead = 10, lastn = T, limits = T, plot = T)
#ase(m27day$f,electricity$x)
#acf(m27day$f)

# Model 3, 
#plotts.sample.wge(electricity$x)
#m3=fore.sigplusnoise.wge(electriaic5city$x,linear=T,freq = c(.1), max.p = 5,n.ahead=100)
#ase(m3$f,electricity$x)


# Unit 9
x21 = gen.arma.wge(n=100,phi=c(.3,-.7), theta =-.4, vara = 4, sn=27 )
x21 = x21 + 37

est.arma.wge(x21,p=2,q=1)
mean(x21)


# Difference data
#bond10 = data("eco.corp.bond")
#x = bond10$Close
#y = artrans.wge(x,phi.tr = 1)
#aic5.wge(x)


x = gen.arima.wge(n=200,d=2,phi=c(1.2,-.6),sn=132)
xd1 = artrans.wge(x,phi.tr = 1)
xd2 = artrans.wge(xd1,phi.tr = 1)
aic5.wge(xd2)
est.ar.wge(xd2,p=2)


#overfitting
xd1 = gen.arima.wge(n=200,phi=c(1.2,-.8),d=1,sn=56)
est.ar.wge(xd1,p=6,type="burg")
est.ar.wge(xd1,p=8,type="burg")

#est.ar.wge(bond10$Close,p=6, type="burg")

# 10 runs of dickey fuller
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)
x = gen.arma.wge(200,phi = c(.9))
adf.test(x)


# remove higher order seasonal
d = log(swdelay1$arr_delay)
d1 = artrans.wge(d,phi.tr = 1)
d112 = artrans.wge(d1,phi.tr = c(0,0,0,0,0,0,0,0,0,0,0,1))
est.ar.wge(d112)


# linear regression model
x = gen.sigplusnoise.wge(100, b0 = 0, b1= 0, phi= .95, sn = 28)
t = seq(1,100,1)
df = data.frame(x = x, t= t)
fit = lm(x~t, data = df)
summary(fit)

# linear regression test
miniex = function() {
  for (i in range(10)) {
    x = gen.sigplusnoise.wge(100, b0 = 0, b1= 0, phi= .95) 
    t = seq(1,100,1)
    df = data.frame(x = x, t= t)
    fit = lm(x~t, data = df)
    summary(fit)
  }
}


# Cochran-Orcutt Test
library(orcutt)
# b1 is slope
x = gen.sigplusnoise.wge(100, b0 = 0, b1= 0, phi= .95,sn=21) 
t = seq(1,100,1)
df = data.frame(x = x, t= t)
fit = lm(x~t, data = df)
summary(fit)
cfit = cochrane.orcutt(fit)
summary(cfit)

# 10.13
x = swdelay1$arr_delay
t = seq(1,length(x),1)
df = data.frame(x = x, t= t)
fit = lm(x~t, data = df)
summary(fit)
cfit = cochrane.orcutt(fit)
summary(cfit)


# Model Building
print("Model Building")
x = gen.arma.wge(n=100,phi = c(1.6,-.9),theta = .8,sn=67)
x = x + 10
plotts.sample.wge(x)


# White Noise and Whitening the Residuals
print("White Noise and Whitening the Residuals")
x = gen.aruma.wge(n=200,s=12,phi = c(1.5,-.8),sn=87)
x = x + 50
plotts.sample.wge(x,lag.max = 60)

# difference the model with (1-B12)
y = artrans.wge(x,phi.tr = c(0,0,0,0,0,0,0,0,0,0,0,1))
aic5.wge(y,type="bic")
yest = est.ar.wge(y,p=2)
plotts.sample.wge(yest$res)
acf(yest$res)

#ljung.wge rub twice
ljung.wge(yest$res,p=2)
ljung.wge(yest$res,K=48,p=2)


# airline model test
phi = c(-.36,-.05,-.14,-.11,.04,.09,-.02,.02,.17,.03,-.10,-.38)
#m = fore.aruma.wge(d12, phi=phi, limits= FALSE)

#signal plus noise fitting
data(hadley)
x=hadley
n=length(x)
t=1:n
d=lm(x~t)
xz = x-d$coefficients[1] - d$coefficients[2]*t
plotts.sample.wge(xz)

arz = aic.wge(xz,p=0:6)
y.trans= artrans.wge(hadley,phi.tr = arz$phi)
t.trans = artrans.wge(t,phi.tr = arz$phi) 
fit = lm(y.trans ~t.trans)
summary(fit)

plotts.wge(fit$residuals)
acf(fit$residuals)
ljung.wge(fit$residuals)

# final sig plus noise model
# xt = -.5257 + .0044t + Zt
# (1-.614B+.044B^2-.078B^3 - .026B^4)Zt = at
# wnv = .0103


# generate realization to see if it looks like the original data
gen.sigplusnoise.wge(160,b0=-.5257, b1 = .0044, phi = arz$phi, vara = .0103)

# forecast with sig plus noise model
fore.sigplusnoise.wge(hadley,max.p = 4,n.ahead = 50, limits = FALSE)

#sunspot
data("sunspot.classic")
x = sunspot.classic
plotts.wge(x)

acf(x)
pacf(x)

s2 = est.ar.wge(x,p=2)
mean(x)
acf(s2$res, lag.max = 50)
ljung.wge(s2$res)
ljung.wge(s2$res, K=48)

p = 177/16
f = 1/p

#sunspot with aic
aic5.wge(x,p=0:10,q=0:0)
s8 = est.ar.wge(x,p=8)
s8$phi
s8$var
plotts.wge(s8$res)

# Multi variate regression
setwd("~/GitHub/MSDS-6373-Time-Series")
Bsales = read.csv("./businesssales.csv")
ksfit = lm(sales ~ ad_tv + ad_online + discount, data=Bsales)
aic.wge(ksfit$residuals,p=0:8,q=0)
fit = arima(Bsales$sales,order = c(7,0,0))

## All data with no lag and no trend
ksfit = lm(sales ~ ad_tv + ad_online + discount, data = Bsales)

## AIC picks a p = 7 model
aic.wge(ksfit$residuals,p=0:8, q=0)  

## Find parameters,  x reg are variables from linear fit model
fit=arima(Bsales$sales,order=c(7,0,0),xreg=Bsales[,3:5])
fit

# Creating lagged variables
# With dplyr lag function
df = data.frame(Y = c(1,1,2,3,4,4,5,8),X1 = c(5,6,6,7,7,8,8,9))
df$X1_L1 = dplyr::lag(df$X1,1)
df$X1_L2 = dplyr::lag(df$X1,2)
df

Bsales$ad_tv_l1 = dplyr::lag(Bsales$ad_tv,1)
Bsales$ad_online_l1 = dplyr::lag(Bsales$ad_online,1)
head(Bsales)


# using the ccf function
l = read.csv("whatisthelag.csv")
ccf(l$Y,l$X1,lag.max = 3)


# Bivariate data

x1.25=c( -1.03,  0.11, -0.18, 0.20, -0.99, -1.63, 1.07,  2.26, -0.49, -1.54,  0.45,  0.92,
         -0.05, -1.18,  0.90,  1.17,  0.31,  1.19,  0.27, -0.09,  0.23, -1.91,  0.46,  3.61, -0.03)
x2.25=c( -0.82,  0.54,  1.13, -0.24, -0.77,  0.22,  0.46, -0.03, -0.59,  0.45,  0.59,  0.15,
         0.60,  0.13, -0.04,  0.12, -0.96,  0.23,  1.81, -0.01, -0.95, -0.55, -0.15,  0.71,  0.90)

x1 = x1.25[1:20]
x2 = x2.25[1:20]

## VAR and VARselect are from CRAN package vars
X=cbind(x1,x2)

# VARselect picks p=5 (using AIC)
VARselect(X, lag.max = 6, type = "const",season = NULL, exogen = NULL)
lsfit = VAR(X,p=5,type="const")
summary(lsfit)

preds=predict(lsfit,n.ahead=5)
#preds$fcst$mel[1,1]-[5,1] are the VAR forecasts formenlanoma.  Similar for sunspot.

plot(seq(1,37,1),melanoma, type = "b", ylim = c(0,6))
points(seq(33,37,1),preds$fcst$mel.67[1:5,1],type = "b", pch = 15)
fanchart(preds)

# melanoma incidence and sunspot numbers 1936-1972
melanoma=c(1.0, 0.9, 0.8, 1.4, 1.2, 1.0, 1.5, 1.9, 1.5, 1.5, 1.5, 1.6, 1.8, 2.8, 2.5, 2.5, 2.4, 2.1, 1.9, 2.4, 2.4, 2.6, 2.6, 4.4, 4.2, 3.8, 3.4, 3.6, 4.1, 3.7, 4.2, 4.1, 4.1, 4.0, 5.2, 5.3, 5.3)
sunspot=c(40, 115, 100,  80,  60,  40,  23,  10,  10,  25,  75, 145, 130, 130,  80,  65,  20,  10,   5,  10, 60, 190, 180, 175, 120,  50,  35,  20,  10,  15,  30,  60, 105, 105, 105,  80,  65)

mel.67=melanoma[1:32]
sun.67=sunspot[1:32] 
p.mel=aic.wge(mel.67,p=0:8,q=0:0)
p.mel$p
mel.est=est.ar.wge(mel.67,p=p.mel$p)
fore.arma.wge(mel.67,phi=mel.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)
p.sun=aic.wge(sun.67,p=0:8,q=0:0)
p.sun$p
sun.est=est.ar.wge(sun.67,p=p.sun$p)
fore.arma.wge(sun.67,phi=sun.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)

# VAR and VARselect are from CRAN package vars
X=cbind(mel.67,sun.67)
VARselect(X, lag.max = 6, type = "const",season = NULL, exogen = NULL) #AIC = 5.04
#VARselect picks p=4 (using AIC)
lsfit=VAR(X,p=4,type='const')
preds=predict(lsfit,n.ahead=5)
#preds$fcst$mel[1,1]-[5,1] are the VAR forecasts formenlanoma.  Similar for sunspot.
plot(seq(1,37,1),melanoma, type = "b", ylim = c(0,6))
points(seq(33,37,1),preds$fcst$mel.67[1:5,1],type = "b", pch = 15)
fanchart(preds)

# unit 12 ble on businesssales
bs = read.csv("businesssales.csv")
head(bs)
t=1:100
bs$t = t
bs2 = bs[1:95,]

b1 = bs2$ad_tv
b2 = bs2$sales

# VAR and VARselect are from CRAN package vars
B = cbind(b1, b2)
VARselect(B, lag.max = 6, type = "const", season = NULL, exogen = NULL) 
lsfit = VAR(B,p = 4, type = 'const')
preds = predict(lsfit,n.ahead=5)

#preds$fcst$mel[1,1]-[5,1] are the VAR forecasts for melanoma.  Similar for sunspot.





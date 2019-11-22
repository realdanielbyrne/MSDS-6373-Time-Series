#AR Forcasting
library(tswge)
data(fig6.1nf)
plotts.wge(fig6.1nf)

#ar1
fore.arma.wge(fig6.1nf,phi=.8,n.ahead=20,plot=TRUE, limits= FALSE)

#ar1 -phi
x1 = gen.arma.wge(100,phi=-.8)
fore.arma.wge(x1,phi=-.8,n.ahead=20,plot=TRUE, limits= FALSE)

#ar2
x2 = gen.arma.wge(n=75,phi=c(1.6,-.8),sn=24)
x2 = x2+25
plotts.wge(x2)
fore.arma.wge(x2,phi=c(1.6,-.8),n.ahead=40,limits=FALSE)

x1 = gen.arma.wge(n=75,phi=c(1.6,-.8),sn=24)
fore.arma.wge(x1,phi=c(1.6,-.8),n.ahead=20,limits = FALSE)

#AR(2,1)
x21 = gen.arma.wge(n=75,phi=c(1.6,-.8),theta = -.9, sn=24)
fore.arma.wge(x21,phi=c(1.6,-.8),theta = -.9,n.ahead=20,limits = FALSE)

fore.arma.wge(x21,phi=c(.8),theta = -.9,n.ahead=20,limits = FALSE)

#candian lynx
data(llynx)
plotts.wge(llynx)

fore.arma.wge(llynx,phi=c(1.3,-.7,.1,-.2),n.ahead=20,limits=F)
fore.arma.wge(llynx,phi=c(1.3,-.7,.1,-.2),theta=-.6,n.ahead=20,limits=F)

#phi weights
psi.weights.wge(phi=c(.4,-.6,.8),theta=.5,lag.max=5)

#LIMITS
##AR1 FORECASTS  
fore.arma.wge(fig6.1nf,phi=.8,n.ahead=20,plot=TRUE, limits= TRUE)

##AR21
data(fig6.2nf)
fore.arma.wge(fig6.2nf,phi=c(1.2,-.6),n.ahead=20,limits = T)

# evaluate fit
## last n
f4 = fore.arma.wge(llynx,phi=c(1.3,-.7,.1,-.2),n.ahead=30,lastn=TRUE,limits=FALSE)
ASE = mean((f4$f-llynx[85:114])^2)
ASE

#forecasting with ARIMA
##arima(0,1,0)
x=gen.aruma.wge(n=50,phi=.8,d=1,sn=15)
fore.aruma.wge(x,d=1,n.ahead=20,limits=FALSE)

##arima(1,1,0)
x=gen.aruma.wge(n=50,phi=.8,d=1,sn=15)
fore.aruma.wge(x,phi=.8,d=1,n.ahead=20,limits=FALSE)

##arima (0,2,0)
x=gen.aruma.wge(n=50,phi=.8,d=1,sn=15)
fore.aruma.wge(x,d=2,n.ahead=20,limits=FALSE)

#forecasting seasonal models
## (1-B^4)
x=gen.aruma.wge(n=24,s=4,sn=6)
fore.aruma.wge(x,s=4,n.ahead=8,lastn = FALSE,plot=TRUE,limits = FALSE)
fore.aruma.wge(x,s=4,n.ahead=8,lastn = TRUE,plot=TRUE,limits = FALSE)

## (1-B^4)(1-.8B)
x=gen.aruma.wge(n=24,phi=.8,s=4,sn=6)
fore.aruma.wge(x,s=4,phi=.8,n.ahead=8,lastn = FALSE,plot=TRUE,limits = FALSE)

# forecast
data(airlog)
test = fore.aruma.wge(airlog,s=12,phi=c(-.36,-.05,-.14-.11,.04,.09,-.02,.02,.17,.03,-.1,-.38),d=1,n.ahead=36,lastn = TRUE,plot=TRUE,limits = FALSE)
ASE = mean((test$f-airlog[(144-36+1):144])^2)
ASE

#signal plus noise
x=gen.sigplusnoise.wge(n=50,b0=10,b1=.2,phi=c(.8,-.6))
fore.sigplusnoise.wge(x,linear=TRUE,n.ahead=20,lastn=FALSE,limits=F)


library(quantmod)
library(metRology)
library(MASS)
library(moments)
wislh<-getSymbols('DEXJPUS',src='FRED', 
                  auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
wislh<-1/wislh
names(wislh)<-'TR'
head(wislh,3)
tail(wislh,3)
logret<-diff(log(wislh$TR))
head(logret,3)
# Log returns continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
# Log returns discretos
ret<-exp(logret)-1
round(head(ret,3),6)

#Continuo
logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
logret.m <- apply.monthly(logret,sum)

logret.q <- apply.quarterly(logret,sum)
round(tail(logret.m,3),6)

logret.y <- apply.yearly(logret,sum)

#Discretos

ret.w <- exp(logret.w)-1

ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)

ret.q <- exp(logret.q)-1

ret.y <- exp(logret.y)-1
round(tail(ret.y,3),6)


mu<-round(mean(logret),8);mu
sig<-round(sd(logret),8);sig


#########################################################
###                                                   ###
###                   Examen 2                        ###
###                                                   ###
#########################################################

wislh<-getSymbols('DEXUSUK',src='FRED', 
                  auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
#wislh<-1/wislh
names(wislh)<-'TR'
head(wislh,3)
tail(wislh,3)
logret<-diff(log(wislh$TR))
head(logret,3)
# Log returns continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
# Log returns discretos
ret<-exp(logret)-1
round(head(ret,3),6)

#Continuo
logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
logret.m <- apply.monthly(logret,sum)

logret.q <- apply.quarterly(logret,sum)
round(tail(logret.m,3),6)

logret.y <- apply.yearly(logret,sum)

#Discretos

ret.w <- exp(logret.w)-1

ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)

ret.q <- exp(logret.q)-1

ret.y <- exp(logret.y)-1
round(tail(ret.y,3),6)


mu<-round(mean(logret),8);mu
sig<-round(sd(logret),8);sig











# MOdelo 1  considerando mu y sigma
var<-round(qnorm(0.05,mu,sig,),6);var
#Valor en riesgo en un día 
1000*(exp(var)-1)
# Especting Shortfall
ES<-mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05;ES
#Máxima perdida en un día 
1000*(exp(ES)-1)


## Modelo 2 Simulación considerando normalidad de retornos 1
alpha<-0.05
set.seed(123789)
#RNGkind(sample.kind="Rounding")
rvec<-rnorm(100000,mu, sig)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## Modelo 3 Simulación de datos actuales retornos 2
alpha<-0.05
set.seed(123789)
rvec<-sample(as.vector(logret),100000,replace=TRUE)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

1000*(exp(ES)-1)

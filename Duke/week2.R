library(quantmod)
wislh<-getSymbols('DEXSZUS',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
wislh<-1/wislh
names(wislh)<-'TR'
logret<-diff(log(wislh$TR))
logret<-diff(log(wislh$TR))[-1]
options(scipen = 999)
mu<-round(mean(logret),8);mu
sig<-round(sd(logret),8);sig
VaR<-round(qnorm(0.01,mu,sig),6);VaR
ES<-mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01;ES

alpha<-0.01
set.seed(123789)
rvec<-rnorm(100000,mu, sig)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
var1<-round(VaR,6);var1
es1<-round(ES,6);es1

#alpha<-0.01
#set.seed(123789)
rvec<-sample(as.vector(logret),100000,replace=TRUE)
VaR <- quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
var2<-round(VaR,6);var2
es2<-round(ES,6);es2



library(metRology)
## Máxima verisimilitud
library(MASS)
t.fit<-fitdistr(rvec,'t')
round(t.fit$estimate,6)

##Estimación del VAR y ES para t-student
rvec<-rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
var3<-round(VaR,6);var3
es3<-round(ES,6);es3

results<-c(mu,sig,var1,es1,var2,es2,var3,es3);results


1000*(exp(-0.022463)-1)


# Log returns discretos
ret<-exp(logret)-1
round(head(ret,3),6)

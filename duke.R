library(quantmod)
wislh<-getSymbols('WILL5000IND',src='FRED', 
                  auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-30']
names(wislh)<-'TR'
head(wislh,3)
tail(wislh,3)
logret<-diff(log(wislh$TR))
head(logret,3)
# Log returns continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
mu<-round(mean(logret),8)
sig<-round(sd(logret),8)

# MOdelo 1  considerando mu y sigma
var<-round(qnorm(0.05,mu,sig,),6)
#Valor en riesgo en un día 
1000*(exp(var)-1)
# Especting Shortfall
ES<-mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05
#Máxima perdida en un día 
1000*(exp(ES)-1)

## Modelo 2 Simulación considerando normalidad de retornos 1
alpha<-0.05
set.seed(123789)
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

## Modelo 4 Simulación de escala de 

library(metRology)
## Máxima verisimilitud
library(MASS)
t.fit<-fitdistr(rvec,'t')
round(t.fit$estimate,6)

##Estimación del VAR y ES para t-student
rvec<-rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)


## A) Simulación para estimar t-student

rvec<-rep(0,100000)
for(i in 1:10){
  rvec<-rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## B) IID simulación 

rvec<-rep(0,100000)
for(i in 1:10){
  rvec<-rvec+sample(as.vector(logret),100000,replace=TRUE)
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)


## C) Block simulación desde una distribución empírica
rvec<-rep(0,100000)
rdat<-as.vector(logret)
posn<-seq(1,length(rdat)-9, by=1)
rpos<-sample(posn,100000, replace = TRUE)
for(i in 1:10){
  rvec<-rvec+rdat[rpos]
  rpos<-rpos+1
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)










## Coeficiente de skewness
library(moments)

rvec<-as.vector(logret)
round(skewness(rvec),2)

## Kurtosis
rvec<-as.vector(logret)
round(kurtosis(rvec),2)

## Jarque-Bera test para normalidad
jarque.test(rvec) #Rechazar la normalidad








# Retornos discreta
ret<-exp(logret)-1
round(head(ret,3),6)
# Retornos semanales
logret_w<-apply.weekly(wislh, sum)
round(head(logret_w,3),6)
ret_w<-exp(logret_w)-1


library(quantmod)
getSymbols("WILL5000IND",src="FRED")
wilsh <- na.omit(WILL5000IND)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wislh))[-1]

logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
round(tail(logret.w,3),6)

logret.m <- apply.monthly(logret,sum)
round(head(logret.m,3),6)
round(tail(logret.m,3),6)

logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)

logret.y <- apply.yearly(logret,sum)
round(head(logret.y,3),6)
round(tail(logret.y,3),6)

ret.w <- exp(logret.w)-1
round(head(ret.w,3),6)
round(tail(ret.w,3),6)
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
ret.q <- exp(logret.q)-1
round(head(ret.q,3),6)
round(tail(ret.q,3),6)
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)

## Actividad 4

wislh<-getSymbols('GOLDPMGBD228NLBM',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'
logret <- diff(log(wislh))[-1] #Retornos diarios
### Continuos ###
# Retornos semanales continuos
round(head(logret,3),6)
mu<-round(mean(logret),8);mu

sig<-round(sd(logret),8);sig

# MOdelo 1  considerando mu y sigma
var<-round(qnorm(0.05,mu,sig,),6);var
#Valor en riesgo en un día 
1000*(exp(var)-1)
# Especting Shortfall
ES<-mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05
#Máxima perdida en un día 
1000*(exp(ES)-1)




## Modelo 2 Simulación considerando normalidad de retornos 1
alpha<-0.05
set.seed(123789)
rvec<-rnorm(100000,mu, sig)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## Modelo 3 Simulación de datos actuales retornos 2
alpha<-0.05
set.seed(123789)
rvec<-sample(as.vector(logret),100000,replace=TRUE)
VaR <- quantile(rvec,0.05)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)


## Modelo 4 Simulación de escala de 

library(metRology)
## Máxima verisimilitud
library(MASS)
t.fit<-fitdistr(rvec,'t')
round(t.fit$estimate,6)

##Estimación del VAR y ES para t-student
rvec<-rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)


## A) Simulación para estimar t-student

rvec<-rep(0,100000)
for(i in 1:10){
  rvec<-rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## B) IID simulación 

rvec<-rep(0,100000)
for(i in 1:10){
  rvec<-rvec+sample(as.vector(logret),100000,replace=TRUE)
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)


## C) Block simulación desde una distribución empírica
rvec<-rep(0,100000)
rdat<-as.vector(logret)
posn<-seq(1,length(rdat)-9, by=1)
rpos<-sample(posn,100000, replace = TRUE)
for(i in 1:10){
  rvec<-rvec+rdat[rpos]
  rpos<-rpos+1
}
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)






logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
round(tail(logret.w,3),6)
#Retorno mensuales continuos
logret.m <- apply.monthly(logret,sum)
round(head(logret.m,3),6)
round(tail(logret.m,3),6)
#Retornos cuatrimestrales continuos
logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)
#Retornos anuales continuos
logret.y <- apply.yearly(logret,sum)
round(head(logret.y,3),6)
round(tail(logret.y,3),6)

#### Discretos ###

#Retornos semanales discretos
ret.w <- exp(logret.w)-1
round(head(ret.w,3),6)
round(tail(ret.w,3),6)
#Retornos mensuales discretos
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
#Retornos cuatrimestrales discretos
ret.q <- exp(logret.q)-1
round(head(ret.q,3),6)
round(tail(ret.q,3),6)
#Retornos anuales discretos
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)


## Quiz 1 Semana 1 ##

wislh<-getSymbols('DEXJPUS',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-1/wislh
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'


# Quiz 1

logret<-diff(log(wislh))[-1]
round(head(logret,3),6)
round(tail(logret,3),6)

## 1979-12-31  

# Quiz 2
## -0.007728

# Quiz 3
#Retornos mensuales discretos
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
# Quiz 4
## 0.167113
# Quiz 5
#Retornos cuatrimestrales continuos
logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)

# Quiz 6
#Retornos anuales discretos
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)


# Quiz 7


# Quiz 8


## Quiz 2 Semana 1 ##

wislh<-getSymbols('DEXUSUK',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'

# Log returnos diarios continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
# Retornos diario discreta
ret<-exp(logret)-1
round(head(ret,3),6)

# Retornos semanales continuos
logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
round(tail(logret.w,3),6)
#Retorno mensuales continuos
logret.m <- apply.monthly(logret,sum)
round(head(logret.m,3),6)
round(tail(logret.m,3),6)
#Retornos cuatrimestrales continuos
logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)
#Retornos anuales continuos
logret.y <- apply.yearly(logret,sum)
round(head(logret.y,3),6)
round(tail(logret.y,3),6)

#### Discretos ###

#Retornos semanales discretos
ret.w <- exp(logret.w)-1
round(head(ret.w,3),6)
round(tail(ret.w,3),6)
#Retornos mensuales discretos
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
#Retornos cuatrimestrales discretos
ret.q <- exp(logret.q)-1
round(head(ret.q,3),6)
round(tail(ret.q,3),6)
#Retornos anuales discretos
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)


## Quiz 3 Semana 1 ##

wislh<-getSymbols('DEXSZUS',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-1/wislh
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'

# Log returnos diarios continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
# Retornos diario discreta
ret<-exp(logret)-1
round(head(ret,3),6)

# Retornos semanales continuos
logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
round(tail(logret.w,3),6)
#Retorno mensuales continuos
logret.m <- apply.monthly(logret,sum)
round(head(logret.m,3),6)
round(tail(logret.m,3),6)
#Retornos cuatrimestrales continuos
logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)
#Retornos anuales continuos
logret.y <- apply.yearly(logret,sum)
round(head(logret.y,3),6)
round(tail(logret.y,3),6)

#### Discretos ###

#Retornos semanales discretos
ret.w <- exp(logret.w)-1
round(head(ret.w,3),6)
round(tail(ret.w,3),6)
#Retornos mensuales discretos
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
#Retornos cuatrimestrales discretos
ret.q <- exp(logret.q)-1
round(head(ret.q,3),6)
round(tail(ret.q,3),6)
#Retornos anuales discretos
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)


## Quiz 3 Semana 1 ##

wislh<-getSymbols('DEXUSAL',src='FRED',auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'

# Log returnos diarios continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)
# Retornos diario discreta
ret<-exp(logret)-1
round(head(ret,3),6)

# Retornos semanales continuos
logret.w <- apply.weekly(logret,sum)
round(head(logret.w,3),6)
round(tail(logret.w,3),6)
#Retorno mensuales continuos
logret.m <- apply.monthly(logret,sum)
round(head(logret.m,3),6)
round(tail(logret.m,3),6)
#Retornos cuatrimestrales continuos
logret.q <- apply.quarterly(logret,sum)
round(head(logret.q,3),6)
round(tail(logret.q,3),6)
#Retornos anuales continuos
logret.y <- apply.yearly(logret,sum)
round(head(logret.y,3),6)
round(tail(logret.y,3),6)

#### Discretos ###

#Retornos semanales discretos
ret.w <- exp(logret.w)-1
round(head(ret.w,3),6)
round(tail(ret.w,3),6)
#Retornos mensuales discretos
ret.m <- exp(logret.m)-1
round(head(ret.m,3),6)
round(tail(ret.m,3),6)
#Retornos cuatrimestrales discretos
ret.q <- exp(logret.q)-1
round(head(ret.q,3),6)
round(tail(ret.q,3),6)
#Retornos anuales discretos
ret.y <- exp(logret.y)-1
round(head(ret.y,3),6)
round(tail(ret.y,3),6)




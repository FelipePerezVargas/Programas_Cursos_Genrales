wislh<-getSymbols('GOLDPMGBD228NLBM',src='FRED', 
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
#options(scipen = 999)

library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)


library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, 't')
round(t.fit$estimate,6)

alpha <- 0.05
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Método 1
alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES 


#Método 2
alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES

#Método 3
alpha <- 0.05
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES








wislh<-getSymbols('WILL5000IND',src='FRED', 
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
#options(scipen = 999)

library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)



############################################################
###                                                      ###
###                       WEEK 3                         ###
###                                                      ###
############################################################


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
#options(scipen = 999)

library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)


library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, 't')
round(t.fit$estimate,6)

alpha <- 0.01
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Método 1
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES 


#Método 2
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES

#Método 3
alpha <- 0.01
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES




wislh<-getSymbols('DEXUSAL',src='FRED', 
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
#options(scipen = 999)

library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)


library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, 't')
round(t.fit$estimate,6)

alpha <- 0.01
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Método 1
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES 


#Método 2
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES

#Método 3
alpha <- 0.01
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha);VaR
ES <- mean(rvec[rvec<VaR]);ES






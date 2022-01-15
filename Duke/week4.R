###################################################
###                                             ###
###                   WEEK 4                    ###
###                                             ###
###################################################


library(quantmod)
library(rugarch)
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

plot(logret)

acf(logret)
acf(abs(logret))


garch.N<-ugarchspec(variance.model =list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                    distribution.model = 'norm')

fit.garch.N<-ugarchfit(spec = garch.N,data = logret)


save1<-cbind(logret,fit.garch.N@fit$sigma,fit.garch.N@fit$z)

#Los retornos diarios
#Los valores ajustados de sqrt(h_{t})
#Los valores ajustados de \epsilon_{t}


garch.t<-ugarchspec(variance.model =list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                    distribution.model = 'std')

fit.garch.t<-ugarchfit(spec = garch.N,data = logret)


save1<-cbind(logret,fit.garch.N@fit$sigma,fit.garch.N@fit$z)
names(save1)<-c('logret','s','z')
parm1<-fit.garch.t@fit$coef

#################### Ejercicio ##############################

wislh<-getSymbols('GOLDPMGBD228NLBM',src='FRED', 
                  auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'
head(wislh,3)
tail(wislh,3)
logret<-diff(log(wislh$TR))
head(logret,3)
# Log returns continuos
logret<-diff(log(wislh$TR))[-1]
round(head(logret,3),6)

plot(logret)

acf(logret)
acf(abs(logret))


garch.N<-ugarchspec(variance.model =list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                    distribution.model = 'norm')

fit.garch.N<-ugarchfit(spec = garch.N,data = logret)


save1<-cbind(logret,fit.garch.N@fit$sigma,fit.garch.N@fit$z)

#Los retornos diarios
#Los valores ajustados de sqrt(h_{t})
#Los valores ajustados de \epsilon_{t}


garch.t<-ugarchspec(variance.model =list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                    distribution.model = 'std')

fit.garch.t<-ugarchfit(spec = garch.t,data = logret[,1])


save1<-cbind(logret[,1],fit.garch.t@fit$sigma,fit.garch.t@fit$z)
names(save1)<-c('logret','s','z')
parm1<-fit.garch.t@fit$coef
parm1
acf(save1$z)
acf(abs(save1$z))



###################### Otro ejercicio ######################



getSymbols("GOLDPMGBD228NLBM",src="FRED")
wilsh <- na.omit(GOLDPMGBD228NLBM)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c('logret', 's', 'z' )

set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,
                         method=c('Partial',"Full")[1], # ignore parameter uncertainty
                         sampling="raw",       # draw from standardized residuals
                         n.ahead=1,            # 1-day ahead
                         n.bootpred=100000,    # number of simulated outcomes
                         solver='solnp')


rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05);VaR
ES <- mean(rvec[rvec<VaR]);ES





set.seed(123789)
boot.garch<-ugarchboot(fit.garch.t,
                       method = 'Partial',
                       sampling = 'raw',
                       n.ahead = 1,
                       n.bootpred =100000,
                       solver = 'solnp' )

rvec<-boot.garch@fseries
alpha<-0.05
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])



n2016<-length(logret['1980-01-01/2016-12-31'])
roll.garch<-ugarchroll(spec = garch.t,
                       data = logret,
                       n.ahead = 1,
                       forecast.length = 1,
                       n.start = n2016,
                       refit.every = 1,
                       refit.window = 'recursive',
                       calculate.VaR = TRUE,
                       VaR.alpha = 0.05,
                       keep.coef = TRUE)




#################################################################
#####                                                       #####
#####                   QUIZ WEEK 4                         #####
#####                                                       #####
#################################################################




wislh<-getSymbols('DEXSZUS',src='FRED', 
                  auto.assign = FALSE )
wislh<-na.omit(wislh)
wislh<-1/wislh
wislh<-wislh['1979-12-31/2017-12-31']
names(wislh)<-'TR'
logret<-diff(log(wislh$TR))
# Log returns continuos
logret<-diff(log(wislh$TR))[-1]
plot(logret)

acf(logret)
acf(abs(logret))



garch.t<-ugarchspec(variance.model =list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                    distribution.model = 'std')

fit.garch.t<-ugarchfit(spec = garch.t,data = logret)


save1<-cbind(logret,fit.garch.t@fit$sigma,fit.garch.t@fit$z)
names(save1)<-c('logret','s','z')
parm1<-fit.garch.t@fit$coef
parm1
acf(save1$z)
acf(abs(save1$z))


set.seed(123789)
boot.garch<-ugarchboot(fit.garch.t,
                       method = 'Partial',
                       sampling = 'raw',
                       n.ahead = 1,
                       n.bootpred =100000,
                       solver = 'solnp' )

rvec<-boot.garch@fseries
alpha<-0.05
VaR<-quantile(rvec,alpha);VaR
ES<-mean(rvec[rvec<VaR]);ES

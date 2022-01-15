data(galton)


library(reshape2)
library(manipulate)
library(ggplot2)
library(plotly)
library(UsingR)


mh<-function(mu){
  mse<-mean(galton$child-mu)^{2}
  g<-ggplot(galton, aes(x=child))+geom_histogram(fill='salmon', colour='black', binwidth=1)
  g<-g+geom_vline(xintercept = mu, size=3)
  g<-g+ggtitle(paste('mu=', mu,'MSE=', round(mse,2), sep=''))
  g<-ggplotly(g, dynamicTicks = TRUE)
}
mh(0.25)
g<-ggplotly(g, dynamicticks = TRUE)
g

manipulate(mh(mu), mu=slider(62,74, step = 0.5))



g<-ggplot(galton, aes(x=child))+geom_histogram(fill='salmon', colour='black',
                                               binwidth=1)
g<-g+geom_vline(xintercept = mean(galton$child), size=3)
g



ggplot(galton, aes(x=parent, y=child))+geom_point()




library(reshape2)
library(manipulate)
library(ggplot2)
library(plotly)
library(UsingR)


mh<-function(mu){
  mse<-mean(galton$child-mu)^{2}
  g<-ggplot(galton, aes(x=child))+geom_histogram(fill='salmon', colour='black', binwidth=1)
  g<-g+geom_vline(xintercept = mu, size=3)
  g<-g+ggtitle(paste('mu=', mu,'MSE=', round(mse,2), sep=''))
  g
}

#g<-ggplotly(g, dynamicTicks = TRUE)
#g

manipulate(mh(mu), mu=slider(62,74, step = 0.5))




x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu<-mean(x);mu
sum(w*(x-mu)^{2})


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)


data(mtcars) 
lm(mtcars$mpg~mtcars$wt)


x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

(x-mean(x))/sd(x)



x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)



plot(a2$Volatilidad,a2$Rendimiento, xlab='Volatilidad', ylab = 'Rendimiento')

labels <- colnames(a2)


p<-c(0.30,0.5,0.2)
a<-c(10,15,25)
b<-c(-5,20,30)
c<-c(-3,5,15)

era<-sum(p*a);era
erb<-sum(p*b);erb
erc<-sum(p*c);erc



p1<-c(0.30,0.5,0.2,10,15,25,-5,20,30,-3,5,15)


erx<-c(round(era,3),round(erb,3),round(erc,3));erx
por<-matrix(p1, ncol=4, nrow=3, byrow = F);por

p2<-c(-20,5,40,-5,10,15,5,3,2)
por1<-matrix(p2, ncol=3, nrow=3, byrow = F);por1
cor(por1)

era<-sum(p*a);era
erb<-sum(p*b);erb
erc<-sum(p*c);erc

cov<-sum(p*(a-15.5)*(c-4.6));cov
cov<-sum(p*(a-15.5)*(b-14.5));cov
cov<-sum(p*(c-4.6)*(b-14.5));cov

sa<-sum(p*(a-era)*(b-erb));sa
sb<-sum(p*(b-erb));sb


da<-sum(p*(a-era)^{2});da
db<-sum(p*(b-erb)^{2});db

cor<-sa/(da*db);cor

#retorno del portafolio


library(statsr)
library(dplyr)
library(ggplot2)

a<-data(mlb11)


plot(a$at_bats)


library(datasets)
data(iris)

?iris

iris
a<-iris$Sepal.Length
mean(a[100:150,])

apply(iris[,1:4],1,mean)
apply(iris[,1:4],2,mean)


data(mtcars)
?mtcars

abs(mean(mtcars$cyl))









library(ggplot2)
library(plotly)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Grafica de densidad", 
       subtitle="Número de Cilindros",
       caption="Fuentee: mpg",
       x="Ciudad",
       fill="# Cylinders")

###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣

library(lubridate)

ggplot(data = diamonds, aes(x = carat, y = price, color = cut)) + 
  labs(title = "Precio vs. Quilates", 
       subtitle = "¿Cuál es la corelación entr el precio y los quilates? ",
       x = "Carat", 
       y = "Price",
       caption = "www.datatoinsight.io" ) + 
  scale_color_manual(values=c('#25AAE2','#F2B53A', '#8BC540', '#DC5D42', '#666666', '9FAFBE')) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  geom_smooth(alpha = 0.2, size = 1.5, span = 4, se=FALSE) + 
  theme(legend.key = element_rect(fill = "white")) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  theme_elegante()

###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣


###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣


###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣###♣
























                   
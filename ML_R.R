browseURL('http://cran.r-project.org/web/views/')
library()
rm(list=(ls))
library(help = 'datasets')
data()
? iris
str(iris)
scan()

################ Load data sets ################################

############ Archivos .txt #####################
Producto<-read.table('C:/Users/Admin/Documents/Cronodata/Cient?fico de datos/Product.txt', header = TRUE, sep = '\t')
View(Producto)
str(Producto)

############### Archivos csv ##################

Costumer<-read.csv('C:/Users/Admin/Documents/Cronodata/Cient?fico de datos/Customer.csv', header = TRUE)
View(Costumer)
str(Costumer)

y<-table(Costumer$Region);y
View(y)
barplot(y)
barplot(y[order(y)])
barplot(y[order(-y)])
barplot(y[order(y)], horiz = TRUE, col = 'red')
barplot(y[order(y)], horiz = TRUE, col = c('red','blue','green','yellow'))
colors()
barplot(y[order(y)], horiz = TRUE, col = c('red','blue','green','yellow'),
        border = NA)
barplot(y[order(y)], horiz = TRUE, col = c('red','blue','green','yellow'),
        border = NA, main = 'Frecuencia de \n regiones', xlab = 'N?mero de Clientes',
        )

############### Histogramas #####################

hist(Costumer$Age)
hist(Costumer$Age, breaks = 5)
hist(Costumer$Age, breaks = c(0,40,60,100))
hist(Costumer$Age, breaks = c(0,40,60,100), freq = TRUE)
hist(Costumer$Age, breaks = c(0,40,60,100), freq = TRUE, col='blue')
hist(Costumer$Age, breaks = c(0,40,60,100), freq = TRUE, col=c('blue','red','green'),
     main = 'Histograma Edad', xlab = 'Edad', ylab = 'Frecuencia')


df<-read.csv('C:/Users/Admin/Documents/Cronodata/Cient?fico de datos/House_Price.csv', header = TRUE)
str(df)
summary(df)

hist(df$crime_rate)

  
  pairs(~df$price+df$crime_rate+df$n_hot_rooms+df$rainfall)
  pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df)
  barplot(table(df$airport))
  barplot(table(df$waterbody))
  barplot(table(df$bus_ter))
  
  uv<-3*quantile(df$n_hot_rooms, 0.99);uv
  df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
  summary(df$n_hot_rooms)
  
  lv<-0.3*quantile(df$rainfall, 0.01)
  df$rainfall[df$rainfall<lv]<-lv
  summary(df$rainfall)
  View(df)
  
  
  mean(df$n_hos_beds)
  mean(df$n_hos_beds, na.rm = TRUE)
  which(is.na(df$n_hos_beds))
  df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds, na.rm = TRUE)
  summary(df$n_hos_beds)
  which(is.na(df$n_hos_beds))
  
  
  pairs(~price+crime_rate, data=df )
  plot(df$price,df$crime_rate)
  plot(df$crime_rate,df$price)
  
  df$crime_rate<-log(1+df$crime_rate)
  plot(df$price,df$crime_rate)
  
  df$avg_dist<- (df$dist1+df$dist2+ df$dist3+ df$dist4)/4
  
  # Eliminar columnas
  df2<-df[, -7:-10]
  df<-df2
  rm(df2)
  df<-df[, -14]
  
  
  
  # Variables dummies  conversion
  
  library(dummies)
  df<-dummy.data.frame(df)
  df<-df[,c(-9,-14)]
  
  # Correlaci?n entre el data frame
  
  round(cor(df),2)
  
  # Simple linear model 
  
  slm<-lm(price~room_num, data=df);slm
  plot(df$room_num, df$price)
  abline(slm, col='red')
  
  mlm<-lm(df$price~df$crime_rate+df$resid_area+df$air_qual+df$room_num+df$age+df$teachers+
            df$poor_prop+df$airportYES+df$n_hos_beds+df$n_hot_rooms+df$waterbodyLake+df$waterbodyNone+
            df$waterbodyRiver+df$rainfall+df$parks+df$avg_dist);mlm
  
  
  summary(mlm)
  
  plot(df$room_num, df$price)
  abline(slm)
  mm<-lm(price~.,data=df )
  summary(mm)
  
  
  # Tres train split
  install.packages('caTools')
  library(caTools)
  
  set.seed(0)
  
  split <- sample.split(df,SplitRatio = 0.8)
  training_set <- subset(df,split == TRUE)
  test_set <- subset(df, split == FALSE)
  
  lm_a<-lm(price~., data=training_set)
  summary(lm_a)
  
  train_a <-predict(lm_a, training_set)
  test_a <-predict(lm_a,test_set)
  
  mean((training_set$price-train_a)^2)
  mean((test_set$price-test_a)^2)

install.packages('leaps')
library(leaps)  
df

lm_best <-regsubsets(price~., data=df, nvmax=15)
summary(lm_best)
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
coef(lm_best, 9)
lm_forward<-regsubsets(price~., data=df, nvmax=15, method = 'forward')
summary(lm_forward)



sum(seq(1,365, by=1))
sample(365, 1, replace=FALSE)  

  
# M?todo Ridge and Lasso

install.packages('glmnet')
library(glmnet)


x<-model.matrix(price~.,data=df)[,-1]
y<-df$price
grid<-10^(seq(10,-2, length=100))
grid

lm_ridge<-glmnet(x,y,alpha = 0, lambda = grid)
summary(lm_ridge)
cv_fit<-cv.glmnet(x,y,alpha=0, lambda = grid)
plot(cv_fit)
opt_lambda<-cv_fit$lambda.min
tss<-sum((y-mean(y))^2)
y_a<-predict(lm_ridge,s=opt_lambda, newx=x)
rss<-sum((y_a-y)^2)
rsq<-1-rss/tss  
  
# Lasso

lm_lasso<-glmnet(x,y,alpha = 1, lambda = grid)

# Todo lo dem?s que est? en el m?todo ridge


df1<-read.csv('C:/Users/Admin/Documents/Cronodata/Cient?fico de datos/Data Files/Logistic Reg Dataset/House-Price.csv'
              ,header = TRUE)

summary(df1)

boxplot(df1$n_hot_rooms)

# Seleccionar las correlaciones netre variables deseadas

pairs(~df1$Sold+df1$rainfall)
barplot(table(df1$airport))
barplot(table(df1$bus_ter))

# Rainfall and n_hot_rooms have a outliers
# h_hos_beds has missing values
# Bus_ter is useless

#  Removiendo los outliers

quantile(df1$n_hot_rooms, 0.99)
uv<-3*quantile(df1$n_hot_rooms, 0.99)
df1$n_hot_rooms[df1$n_hot_rooms>uv] <- uv
summary(df1$n_hot_rooms)
boxplot(df1$n_hot_rooms)

lv<-0.3*quantile(df1$rainfall, 0.01)
df1$rainfall[df1$rainfall<lv] <- lv
summary(df1$rainfall)
boxplot(df1$rainfall)

# Removiendo los valores faltantes

mean(df1$n_hos_beds)
mean(df1$n_hos_beds, na.rm=TRUE)

which(is.na(df1$n_hos_beds))
df1$n_hos_beds[is.na(df1$n_hos_beds)] <- mean(df1$n_hos_beds, na.rm=TRUE)
summary(df1$n_hos_beds)
which(is.na(df1$n_hos_beds))

# Tranformaci?n de variables 

df1$avg_dist<- (df1$dist1+df1$dist2+df1$dist3+df1$dist4)/4

#  Quitando los valores innecesarios 
df2<-df1[,-6:-9]
df1<-df2 
rm(df2)
View(df1)
df1<-df1[,-13]


#  Creaci?n de variable dummy

library(dummies)

df1<-dummy.data.frame(df1)
df1<-df1[,c(-8,-13)]


#  Regresi?n log?stica con un solo predictor 

glm.fit<-glm(Sold~price, data=df1, family = binomial)
summary(glm.fit)


#  Regresi?n log?stica con un solo predictor 

glm.fit<-glm(Sold~., data=df1, family = binomial)
summary(glm.fit)


 # Matriz de confusi?n

glm.probs<-predict(glm.fit, type='response')
glm.probs[1:10]

glm.predict<-rep('NO', 506)
glm.predict[glm.probs>0.5]<- 'YES'
table(glm.predict, df1$Sold)

#  An?lisis discriminante lineal 

library(MASS)


lda.fit<-lda(Sold~.,data=df1)
lda.fit
lda.pred<-predict(lda.fit, df1)
lda.pred$posterior
lda.class<-lda.pred$class
table(lda.class,df1$Sold)
sum(lda.pred$posterior[,1]>0.8)



for (i in 1:365){
 print(sample(365,i,replace = FALSE))
}



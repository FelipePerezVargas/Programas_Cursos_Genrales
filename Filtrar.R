library(ROAuth)
library(twitteR)
library(httr)
library(dplyr)
demo<-read.csv('demographics.csv')
View(demo)
# Se selecciona en la calumna de marital solo a los que estan casados
demo2<-filter(demo,marital=='Married')
# Se selecciona a las personas que tengan ingresos mayores a 55
demo2<-filter(demo,income>55 & marital=='Married')
demo2<-filter(demo,income>55 , marital=='Married')

# Se selecciona a los que tengan el tipo de carro luxury
demo2<-filter(demo,income>55 & marital=='Married' & carcat=='Luxury' )
# Selecciona a los que esten entre 35 y 45 años
demo2<-filter(demo, age>=35, age<=45)

demo2<-filter(demo, age<30, age>50)
# Selecciona a los que tengan mas de 55 o esten casados
demo2<-filter(demo, income>=55 | marital=='Married')
# Selección multiple vectorizando %in% 
demo2<-filter(demo,age %in% c(42,45,60))
# Slección invera a la anterior ahora selecciona a los que no tienen 42, 45, 60
demo2<-filter(demo,!age %in% c(42,45,60))


View(demo2)

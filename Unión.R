library(dplyr)


cities<-read.csv('cities.csv')
View(cities)
building<-read.csv('buildings.csv')
View(building)


ij<-inner_join(cities, building)
View(ij)


ij2<-inner_join(building, cities)
View(ij2)


##################  Semi_join ########################

sj<-semi_join(cities, building)
View(sj)


sj2<-semi_join(building, cities)
View(sj2)


##################  Left_join ########################

lj<-left_join(cities, building)
View(lj)


lj2<-left_join(building, cities)
View(lj2)


##################  Anti_join ########################

aj<-anti_join(cities, building)
View(aj)


aj2<-anti_join(building, cities)
View(aj2)


##################  Full_join ########################

fj<-full_join(cities, building)
View(fj)


fj2<-full_join(building, cities)
View(fj2)




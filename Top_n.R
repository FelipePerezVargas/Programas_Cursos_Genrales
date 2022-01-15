library(dplyr)
demo<-read.csv('C:/Users/Admin/Documents/Cronodata/Científico de datos/demographics.csv')


demo2<-top_n(demo, 10, income)
demo2<-top_n(demo, 10, age)


View(demo2)

library(dplyr)
demo<-read.csv('C:/Users/Admin/Documents/Cronodata/Científico de datos/demographics.csv')
tally(demo)

tally(demo, income)
tally(demo, carpr)

demo2<-add_tally(demo)


View(demo2)


demo3<-add_tally(demo, income)


View(demo3)

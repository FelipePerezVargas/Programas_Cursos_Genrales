library(dplyr)
demo<-read.csv('demographics.csv')

demos_s<-sample_n(demo, 100)


demos_s<-sample_n(demo, 100, replace = TRUE)

id<-1:5
names<-c('Tom', 'Mary', 'Nicholas', 'Diana','Ellen')
dat<-data.frame(id,names)

View(dat)

dat_s<-sample_n(dat, 100, replace = TRUE)
View(dat_s)


demo_s2<-sample_frac(demo, 0.3)
demo_s2<-sample_frac(demo, 0.3, replace = TRUE)
View(demo_s2)

library(dplyr)
demo<-read.csv('demographics.csv')

meaninc<-summarise(demo,avginc=mean(income,na.rm = TRUE))
View(meaninc)
typeof(meaninc)

summarise(demo,stdinc=sd(income,na.rm = TRUE))

summarise(demo,suminc=sum(income,na.rm = TRUE))

summarise(demo,medinc=median(income,na.rm = TRUE))

summarise(demo,mininc=min(income,na.rm = TRUE))

summarise(demo,maxinc=max(income,na.rm = TRUE))

summarise(demo,varinc=var(income,na.rm = TRUE))

summarise(demo, n())

summarise(demo, avginc=mean(income,na.rm = TRUE), 
          stdinc=sd(income,na.rm = TRUE), varinc=var(income,na.rm = TRUE), n())

summarise(filter(demo, age>35), avginc=mean(income,na.rm = TRUE), stdinc=sd(income,na.rm = TRUE), n() )


summarise(filter(demo, gender=='Male'), avginc=mean(income,na.rm = TRUE),
          stdinc=sd(income,na.rm = TRUE), n() )

summarise(demo, across(c(1,5), mean))

summarise(demo, across(where(is.numeric), mean))

summarise(filter(demo, carcat=='Luxury'), across(where(is.numeric), sd))


summarise(demo, across(starts_with('c'), mean))

summarise(demo, across(starts_with('c') & where(is.numeric), mean))


demo2<-select(demo, age, income, carpr)
View(demo2)
summarise(demo2, across(everything(), mean))
          
          
          
          
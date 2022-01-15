library(dplyr)
demo<-read.csv('demographics.csv')

# %>%  conexión then


demo2<-demo %>% filter(age<40 & educ=='Some college') 
demo2<-demo %>% select(income, carcat, carpr) 
demo2<-demo %>% filter(age<40 & educ=='Some college') %>% 
             select(income, carcat, carpr) 
demo2<-demo %>% filter(age<40 & educ=='Some college') %>% 
  select(income, carcat, carpr) %>% 
  mutate(x=income/carpr)
demo2<-demo %>% filter(age<40 & educ=='Some college') %>% 
  select(income, carcat, carpr) %>%
  arrange(desc(income))
demo2<-demo %>% filter(age<40 & educ=='Some college') %>% 
            summarise(avginc=mean(income, na.rm=TRUE))   
demo2<-demo %>% filter(age<40 & educ=='Some college') %>% 
  summarise(avginc=mean(income, na.rm=TRUE), stdinc=sd(income, na.rm=TRUE))
View(demo2)


demo %>% group_by(gender) %>%
      filter(carcat=='Luxury') %>%
     summarise(across(where(is.numeric), mean))
    
demo %>% group_by(gender) %>%
  filter(carcat=='Luxury') %>%
  summarise(n())

demo %>% group_by(gender) %>%
  select(carpr, income) %>%
  summarise(across(everything(), mean))
  
demo %>% group_by(gender) %>%
  mutate(x=income/carpr) %>%
  arrange(x)

demo %>% filter(age>40) %>%
     count(marital,gender)


demo %>% filter(age>40) %>%
  tally(income)

demo %>% group_by(marital) %>%
    filter(income>40)%>%
  tally(income)


demo2<- demo %>% filter(carcat=='Standard') %>%
    sample_n(50)

demo2<- demo %>% filter(carcat=='Standard') %>%
        select(age, income, carcat) %>%
  sample_n(50)

demo2<- demo %>% filter(carcat=='Standard') %>%
  select(age, income,carpr, carcat) %>%
  sample_n(50) %>%
top_n(5, income)

demo2<- demo %>% filter(carcat=='Standard') %>%
  select(age, income, carpr, carcat) %>%
  sample_n(50) %>%
  glimpse

demo2




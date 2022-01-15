##################################################
###                                            ###  
###             DATA VISUALIZATION             ###
###                                            ###
##################################################

library(tidyverse)
library(dslabs)
library(ggplot2)
data(murders)
head(murders)
murders$region
length(levels(murders$region))
x<-data(heights)

# determining that the murders dataset is of the "data frame" class
class(murders)
# finding out more about the structure of the object
str(murders)
# showing the first 6 lines of the dataset
head(murders)

# using the accessor operator to obtain the population column
murders$population
# displaying the variable names in the murders dataset
names(murders)
# determining how many entries are in a vector
pop <- murders$population
length(pop)
# vectors can be of class numeric and character
class(pop)
class(murders$state)

# logical vectors are either TRUE or FALSE
z <- 3 == 2
z
class(z)

# factors are another type of class
class(murders$region)
# obtaining the levels of a factor
levels(murders$region)

a<-2;b<--1;c<--4
(-b+sqrt(b**2-4*a*c))/2*a
(-b-sqrt(b**2-4*a*c))/2*a

raices = function(a,b,c){
  if (b^2 - 4*a*c >= 0) {
    raiz1 = (-b + sqrt(b^2 - 4*a*c))/(2*a)
    raiz2 = (-b - sqrt(b^2 - 4*a*c))/(2*a)
  }
  else {
    raiz1 = (-b + sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
    raiz2 = (-b - sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
  }
  c(raiz1, raiz2)
}
raices(2,-1,-4)


data(movielens)


string(data(movielens))


# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]















# make a table of category proportions
prop.table(table(heights$sex))

data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)
print(paste('The numbre of Male is',length(male), 
            'and number of Female is', length(female)))

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

quantile(heights$height, seq(.01, 0.99, 0.01))





data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)


x <- heights%>% filter(sex=="Male") %>% pull(height)
x<-heights$sex
x<-x=='Male'


1 - pnorm(70.5, mean(x), sd(x))

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


fa<-pnorm(x,mean = average,sd = SD)
hist(fa)



data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) - pnorm(69, avg, stdev)




x <- Galton$child

error_avg <- function(k){
  x[1] <- k
  mean(x)
}

error_avg(10000)
error_avg(-10000)


quantile(data,q)
p <- seq(0.01, 0.99, 0.01)
quantile(data, p)
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
qnorm(p, mu, sigma)
qnorm(p)


index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


data(murders)

ggplot(data = murders)

murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

# no error from this call
p_test <- p + geom_text(aes(population/10^6, total, label = abb))

# error - "abb" is not a globally defined variable and cannot be found outside of aes
p_test <- p + geom_text(aes(population/10^6, total), label = abb)


# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))


murders %>% ggplot(aes(population,
                       total, label = abb )) +
  geom_label( color = region)



p<- murders %>% ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()
p + scale_x_log10() + scale_y_log10()


library(dplyr)
library(NHANES)
data(NHANES)
## fill in what is needed
tab <- NHANES %>%filter( AgeDecade =='20-29' & Gender=='female') 
head(NHANES, 3)

tab

# Cargar imagenes para editarlas usando el paquete magick

homer<-image_read("C:/Users/Admin/Desktop/Temporales/Homer.jpg")
print(homer)
image_info(homer)
image_browse(homer)






complete <- function(directory, id=1:332){
      mylist <- list.files(path=directory, pattern=".csv")
      nobs <- numeric()
      for(i in id){
            mydata <- read.csv(mylist[i])
            mysum <- sum(complete.cases(mydata))
            nobs <- c(nobs, mysum)
      }
      data.frame(id, nobs)
}

directory<-"C:/Users/Admin/Desktop/Coursera/specdata"
complete("C:/Users/Admin/Desktop/Coursera/specdata", c(2, 4, 8, 10, 12))

cc <- complete("C:/Users/Admin/Desktop/Coursera/specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("C:/Users/Admin/Desktop/Coursera/specdata", 54)
print(cc$nobs)


RNGversion("3.5.1")  
set.seed(42)
cc <- complete("C:/Users/Admin/Desktop/Coursera/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("C:/Users/Admin/Desktop/Coursera/specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("C:/Users/Admin/Desktop/Coursera/specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("C:/Users/Admin/Desktop/Coursera/specdata", 2000)                
n <- length(cr)                
cr <- corr("C:/Users/Admin/Desktop/Coursera/specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


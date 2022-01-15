corr <- function(directory, threshold = 0){
      mylist <- list.files(path=directory, pattern=".csv")
      df <- complete(directory)
      ids <- df[df["nobs"] > threshold, ]$id
      corrr <- numeric()
      for(i in ids){
            mydata <- read.csv(mylist[i])
            dff <- mydata[complete.cases(mydata), ]
            corrr <- c(corrr, cor(dff$sulfate, dff$nitrate))
      }
      return(corrr)
}

directory<-"C:/Users/Admin/Desktop/Coursera/specdata"
cr<-corr("C:/Users/Admin/Desktop/Coursera/specdata", 150)
head(cr)
summary(cr)



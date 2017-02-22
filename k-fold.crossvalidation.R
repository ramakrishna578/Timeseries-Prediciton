#loading files
library(readr)
m1 <- read.csv("D:/datascience/predictiveanalytics/ANN.data/16hours/combine.i.o.csv", header = T)
# removing common columns or filtering dataframe for desired one
m1$O.wekday <- NULL
m1$O.dayofyear <- NULL
m1$O.nooftransactions <- NULL
m1$O.totalenergy <- NULL
m1$O.maxprice <- NULL
m1$O.minprice <- NULL
library(lubridate)
m1$Timestamp <- as.POSIXct(m1$Timestamp,origin = "1970-01-01", tz="UTC")
m1$Timestamp <- as.numeric(m1$Timestamp)
m1$weekday <- as.numeric(m1$weekday)
m1$dayofyear <- as.numeric(m1$dayofyear)
m1$nooftransactions <- as.numeric(m1$nooftransactions)
apply(m1,2,function(x) sum(is.na(x)))
doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - 
                                                 min(x, na.rm=TRUE))} 
ANN <- as.data.frame(lapply(m1, doit))
#cross validation, using neuralnet to predict 
# dividing data into training, testing, validating
set.seed(584)
cv.error <- NULL
k <- 10

library(MASS)
library(plyr)
library(dplyr)
library(neuralnet)
pbar <- create_progress_bar('text')
pbar$init(k)
#function for k fold
for(i in 1:k){
  index <- sample(1:nrow(ANN),round(0.70*nrow(ANN)))
  training <- ANN[index,]
  test <- ANN[-index,]
  #run a random forest model
  nn <-  neuralnet(O.avgprice+O.firstquartil+ O.thirdquartil~Timestamp+avg.price+weekday+dayofyear+total.energy+firstquartil+thirdquartil+nooftransactions,data = trainANN,hidden = c(4,4), threshold = 0.5,learningrate = 0.5 ,act.fct = "tanh", stepmax = 100000,  linear.output = T)
  # checking and ploting results of neuralnetwork
  prnn <- compute(nn,test[,1:8])
  # o.avgprice
  prnn1 <- prnn$net.result[,1]*(max(m1$O.avgprice)-min(m1$O.avgprice))+min(m1$O.avgprice)
  test1 <- test$O.avgprice*(max(m1$O.avgprice)-min(m1$O.avgprice))+min(m1$O.avgprice)
  MSE.nn[i] <- sum((test1 - prnn1)^2)/nrow(test)
  #o.firstquartil
  prnn2 <- prnn$net.result[,2]*(max(m1$firstquartil)-min(m1$firstquartil))+min(m1$firstquartil)
  test2 <- test$O.firstquartil*(max(m1$firstquartil)-min(m1$firstquartil))+min(m1$firstquartil)
  MSE.nn2[i] <- sum((test2 - prnn2)^2)/nrow(test)
  #o.thirdquartil
  prnn3 <- prnn$net.result[,3]*(max(m1$O.thirdquartil)-min(m1$O.thirdquartil))+min(m1$O.thirdquartil)
  test3 <- test$O.thirdquartil*(max(m1$O.thirdquartil)-min(m1$O.thirdquartil))+min(m1$O.thirdquartil)
  MSE.nn3[i] <- sum((test3 - prnn3)^2)/nrow(test)
  print(paste(MSE.nn,MSE.nn2,MSE.nn3))
  
  pbar$step()
}

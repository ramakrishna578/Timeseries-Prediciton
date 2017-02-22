#loading files
library(readr)
m1 <- read.csv("F:/education/predictiveanalytics/ANN.data/1.33days/totaldataincluded/combine.i.o.ANN.csv", header = T)
# removing common columns or filtering dataframe for desired one
m1$O.weekday <- NULL
m1$O.dayofyear <- NULL
m1$O.nooftransactions <- NULL
m1$O.totalenergy <- NULL
m1$O.maxprice <- NULL
m1$O.minprice <- NULL
#m1$min.price <- NULL
#m1$max.price <- NULL
library(lubridate)
m1$Timestamp <- as.POSIXct(m1$Timestamp,origin = "1970-01-01", tz="UTC")
m1$Timestamp <- as.numeric(m1$Timestamp)
m1$weekday <- as.numeric(m1$weekday)
m1$dayofyear <- as.numeric(m1$dayofyear)
m1$nooftransactions <- as.numeric(m1$nooftransactions)
# designing neural network
set.seed(584)
library(MASS)
# verifying NA's
apply(m1,2,function(x) sum(is.na(x)))
# normalising data
doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - 
                                                 min(x, na.rm=TRUE))} 
ANN <- as.data.frame(lapply(m1, doit))
# dividing data into training, testing, validating
index <- sample(1:nrow(ANN),round(0.70*nrow(ANN)))
trainANN <- ANN[index,]
test <- ANN[-index,]
# designing neuralnetwork
library(neuralnet)
nn <-  neuralnet(O.avgprice+O.firstquartil+O.thirdquartil ~Timestamp+min.price+max.price+avg.price+weekday+dayofyear+total.energy+firstquartil+thirdquartil+nooftransactions,data = trainANN,hidden = c(4,4,4), threshold = 0.5,learningrate = 0.5 ,act.fct = "tanh", stepmax = 1000000,  linear.output = F)
# checking and ploting results of neuralnetwork
nn
plot(nn)
prnn <- compute(nn,test[,1:10])
# o.avgprice
prnn1 <- prnn$net.result[,1]*(max(m1$O.avgprice)-min(m1$O.avgprice))+min(m1$O.avgprice)
test1 <- test$O.avgprice*(max(m1$O.avgprice)-min(m1$O.avgprice))+min(m1$O.avgprice)
MSE.nn <- sum((test1 - prnn1)^2)/nrow(test)
#o.firstquartil
prnn2 <- prnn$net.result[,2]*(max(m1$firstquartil)-min(m1$firstquartil))+min(m1$firstquartil)
test2 <- test$O.firstquartil*(max(m1$firstquartil)-min(m1$firstquartil))+min(m1$firstquartil)
MSE.nn2 <- sum((test2 - prnn2)^2)/nrow(test)
#o.thirdquartil
prnn3 <- prnn$net.result[,3]*(max(m1$O.thirdquartil)-min(m1$O.thirdquartil))+min(m1$O.thirdquartil)
test3 <- test$O.thirdquartil*(max(m1$O.thirdquartil)-min(m1$O.thirdquartil))+min(m1$O.thirdquartil)
MSE.nn3 <- sum((test3 - prnn3)^2)/nrow(test)
print(paste(MSE.nn,MSE.nn2,MSE.nn3))
# plotting difference between predicted and test data
h1 <- prnn1-test1
h2 <- prnn2-test2
h3 <- prnn3-test3
hist(h1,right = TRUE,breaks = 70)
hist(h2,right = TRUE,breaks = 70)
hist(h3,right = TRUE,breaks = 70)
lines(h1)
# plotting
# Graph cars using a y axis that ranges from 0 to 12
plot(prnn1, type="o", col="blue", xlim=c(0,500))

# Graph trucks with red dashed line and square points
lines(test1, type="o", pch=22, lty=2, col="red")
title(main="o.avgprice", col.main="black", font.main=4)
# plotting
# Graph cars using a y axis that ranges from 0 to 12
plot(prnn2, type="o", col="blue", xlim=c(0,500))

# Graph trucks with red dashed line and square points
lines(test2, type="o", pch=22, lty=2, col="red")
title(main="o.firstquartil", col.main="black", font.main=4)

# plotting
# Graph cars using a y axis that ranges from 0 to 12
plot(prnn3, type="o", col="blue", xlim=c(0,500))

# Graph trucks with red dashed line and square points
lines(test3, type="o", pch=22, lty=2, col="red")
title(main="o.thirdquartil", col.main="black", font.main=4)



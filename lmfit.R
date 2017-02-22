#loading files
library(readr)
m1 <- read.csv("D:/datascience/predictiveanalytics/ANN.data/alldatanotincluded/combine.i.o.csv", header = T)
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
m1$nooftransactions <- as.numeric(m1$nooftransactions)
# designing neural network
set.seed(584)
library(MASS)
# verifying NA's
apply(m1,2,function(x) sum(is.na(x)))
# normalising data
ANN <- m1
# dividing data into training, testing, validating
index <- sample(1:nrow(ANN),round(0.80*nrow(ANN)))
training <- ANN[index,]
test <- ANN[-index,]
# dividing traing data into training and validating data
index2 <- sample(1:nrow(training),round(0.70*nrow(training)))
trainANN <- training[index2,]
validateANN <- training[-index2,]
# designing neuralnetwork
lm.fit <- glm(O.thirdquartil ~ Timestamp+min.price+max.price+avg.price+weekday+dayofyear+total.energy+firstquartil+thirdquartil+nooftransactions, data=trainANN)
summary(lm.fit)
pr.lm <- predict(lm.fit,test[,1:10])
MSE.lm <- sum((pr.lm-test$O.thirdquartil )^2)/nrow(test)
plot(pr.lm,test$O.thirdquartil)

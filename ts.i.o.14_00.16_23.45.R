load_data <- function(path){
  files <- dir("D:/datascience/predictiveanalytics/modified/manipulated/all", pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
m1 <- load_data("all")
library(lubridate)
m1$Timestamp.Transaction <- as.POSIXct(m1$Timestamp.Transaction, tz = "UTC")
m1$Timestamp.Delivery <- as.POSIXct(m1$Timestamp.Delivery, tz = "UTC")
m1$D.O.Delivery <- NULL
m1$T.o.Delivery <- NULL
t1 <- unique(m1$Timestamp.Transaction)
c1 <- as.POSIXct("2014-01-01 00:00:00", tz = "UTC")
c2 <- as.POSIXct("2016-12-31 24:00:00", tz = "UTC")
t2 <- subset(t1,t1 >= c1 & t1 <= c2 )
m3 <- lapply(t2, function(x)
  subset(m1$Price, (m1$Timestamp.Transaction <=x & m1$Timestamp.Transaction >= x-115200)&(m1$Timestamp.Delivery == x+14400)))
m4 <- lapply(t2, function(x)
  subset(m1$Energy, (m1$Timestamp.Transaction <=x & m1$Timestamp.Transaction >= x-115200)&(m1$Timestamp.Delivery == x+14400)))
# finding min,max,mean of the price and weekday,dayof the year of timestamp, total energy
Timestamp <- t2
min.price <- unlist(lapply(m3,min))
max.price <- unlist(lapply(m3,max))
avg.price <- unlist(lapply(m3,mean))
weekday <- unlist(lapply(t2,wday)) 
dayofyear <-unlist(lapply(t2,yday))
total.energy <- unlist(lapply(m4,sum))
firstquartil <- unlist(lapply(m3,quantile,probs = 1/4,na.rm=TRUE))
thirdquartil <- unlist(lapply(m3,quantile,probs = 3/4,na.rm=TRUE))
nooftransactions <- unlist(lapply(m3,length))
input.n <- data.frame(Timestamp,min.price,max.price,avg.price,weekday,dayofyear,total.energy,firstquartil,thirdquartil,nooftransactions)
write.csv(input.n, "D:/datascience/predictiveanalytics/in_ts.csv")
library(readr)
load_data <- function(path){
  files <- dir("D:/datascience/predictiveanalytics/modified/manipulated/all", pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
m1 <- load_data("all")
library(lubridate)
m1$Timestamp.Transaction <- as.POSIXct(m1$Timestamp.Transaction, tz = "UTC")
m1$Timestamp.Delivery <- as.POSIXct(m1$Timestamp.Delivery, tz = "UTC")
m1$D.O.Delivery <- NULL
m1$T.o.Delivery <- NULL
t1 <- unique(m1$Timestamp.Transaction)
c1 <- as.POSIXct("2014-01-01 00:00:00", tz = "UTC")
c2 <- as.POSIXct("2016-12-31 24:00:00", tz = "UTC")
t2 <- subset(t1,t1 >= c1 & t1 <= c2 )
m2 <- lapply(t2, function(x)
  subset(m1, (m1$Timestamp.Transaction >=x & m1$Timestamp.Transaction <= x+14400)&(m1$Timestamp.Delivery == x+14400)))
m3 <- lapply(t2, function(x)
  subset(m1$Price, (m1$Timestamp.Transaction >= x & m1$Timestamp.Transaction <= x+14400)&(m1$Timestamp.Delivery == x+14400)))
m4 <- lapply(t2, function(x)
  subset(m1$Energy, (m1$Timestamp.Transaction >= x & m1$Timestamp.Transaction <= x+14400)&(m1$Timestamp.Delivery == x+14400)))
# finding min,max,mean of the price and weekday,dayof the year of timestamp, total energy
Timestamp <- t2
min.price <- unlist(lapply(m3,min))
max.price <- unlist(lapply(m3,max))
avg.price <- unlist(lapply(m3,mean))
weekday <- unlist(lapply(t2,wday)) 
dayofyear <-unlist(lapply(t2,yday))
total.energy <- unlist(lapply(m4,sum))
firstquartil <- unlist(lapply(m3,quantile,probs = 1/4,na.rm=TRUE))
thirdquartil <- unlist(lapply(m3,quantile,probs = 3/4,na.rm=TRUE))
nooftransactions <- unlist(lapply(m3,length))
input.n <- data.frame(Timestamp,min.price,max.price,avg.price,weekday,dayofyear,total.energy,firstquartil,thirdquartil,nooftransactions)
write.csv(input.n, "D:/datascience/predictiveanalytics/out_ts.csv")
 
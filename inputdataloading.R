# reading csv data into r
# libraries required for loading csv files
library(readr)
library(utils)
# allocating loaded csv data into df m1
m1 <- read.csv("D:/datascience/predictiveanalytics/rawdata/2016/intraday_transactions_germany_austria_2016-10.csv", header=T, sep = ";", dec = ",",colClasses=rep("factor"),  stringsAsFactors = F,na = "NA")
# to view structure of m1
structure(m1)
head(m1)
m1$Volume..MW.
# Removing columns
m1$Market.Area.Buy <- NULL
m1$Market.Area.Sell <- NULL
structure(m1)
head(m1)
tail(m1)
# converting character to date(POSIXT)
library(lubridate)
m1$Time.Stamp <- dmy_hms(m1$Time.Stamp, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"),truncated = 0)
# rounding timestamp to 15min
m1$Time.Stamp <- round_date(m1$Time.Stamp,"15mins")
#checking class of vectors
class(m1$Date)
class(m1$Time.Stamp)
class(m1$Volume..MW.)
class(m1$Price..EUR.)
class(m1$Hour.from)
class(m1$Hour.to)
head(m1)
tail(m1)
# converting characters into integers(time)
m1$Hour.from <- gsub("A", "", m1$Hour.from)
m1$Hour.from <- gsub("B", "", m1$Hour.from)
m1$Hour.from <- gsub("qh1", ".00", m1$Hour.from)
m1$Hour.from <- gsub("qh2", ".15", m1$Hour.from)
m1$Hour.from <- gsub("qh3", ".30", m1$Hour.from)
m1$Hour.from <- gsub("qh4", ".45", m1$Hour.from)

m1$Hour.to <- gsub("A", "", m1$Hour.to)
m1$Hour.to <- gsub("B", "", m1$Hour.to)
m1$Hour.to <- gsub("qh1", ".15", m1$Hour.to)
m1$Hour.to <- gsub("qh2", ".30", m1$Hour.to)
m1$Hour.to <- gsub("qh3", ".45", m1$Hour.to)
m1$Hour.to <- gsub("qh4", ".00", m1$Hour.to)
m1$Hour.from
m1$Hour.to
# conversion of character to numeric
m1$Hour.from <- as.numeric(m1$Hour.from)
m1$Hour.to <- as.numeric(m1$Hour.to)
m1
# check which is NA
which(is.na(m1$Hour.from))
which(is.na(m1$Hour.to))

# creating new column for total time of delivery
m1$TTD <- m1$Hour.from - m1$Hour.to
m1$TTD <- abs(m1$TTD)
m1$TTD <- as.factor(m1$TTD)
m1$TTD[m1$TTD == 0.00] <- 1.00
m1$TTD[m1$TTD == "0.45"] <- 0.15
m1$TTD[m1$TTD == 0.149999999999999] <- 0.15
m1$TTD[m1$TTD ==   0.449999999999999] <- 0.15
m1$TTD[m1$TTD ==   0.150000000000002] <- 0.15

# creating power= volume*ttd
m1$Volume..MW. <- as.numeric(as.character(sub("," , ".", m1$Volume..MW.)))
m1$Price..EUR. <- as.numeric(as.character(sub("," , ".", m1$Price..EUR.)))
m1$TTD <- as.numeric(as.character(sub("," , ".", m1$TTD)))
m1$Energy <- m1$TTD * m1$Volume..MW.
# converting hour from to time
m1$Hour.to <- NULL
m1$Hour.from <-  gsub('[.]', ':', sprintf('%0.2f.00', m1$Hour.from))
m1$Date <- as.character(m1$Date)
m1$T.SD <- paste(m1$Date,m1$Hour.from,sep = " ",collapse = NULL)
library(lubridate)
m1$T.SD <-  dmy_hms(m1$T.SD, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"),truncated = 0)

# changing column names
colnames(m1)[2] <- "TOD"
colnames(m1) <- c("D.O.Delivery", "T.o.Delivery","Power","Price","Timestamp.Transaction","Total.time.Delivery","Energy","Timestamp.Delivery")
# writing data into csv, excel, table
write.csv(m1, "D:/datascience/predictiveanalytics/2016-10.csv",row.names = F)



#####import libraries#####
library(tseries)
library(forecast)

#####load data######
#train data
traffic <- read.csv("traffic/train_ML_IOT.csv")
j1 <- traffic[traffic$Junction == 1,]
j2 <- traffic[traffic$Junction == 2,]
j3 <- traffic[traffic$Junction == 3,]
j4 <- traffic[traffic$Junction == 4,]

#test data
traffic_test <- read.csv("traffic/test_ML_IOT.csv")

j1_test <- traffic_test[traffic_test$Junction == 1,]
j2_test <- traffic_test[traffic_test$Junction == 2,]
j3_test <- traffic_test[traffic_test$Junction == 3,]
j4_test <- traffic_test[traffic_test$Junction == 4,]

#convert character dates to actual calendar date time type
j1$DateTime <- strptime(j1$DateTime, format="%Y-%m-%d %H:%M:%S")
j2$DateTime <- strptime(j2$DateTime, format="%Y-%m-%d %H:%M:%S")
j3$DateTime <- strptime(j3$DateTime, format="%Y-%m-%d %H:%M:%S")
j4$DateTime <- strptime(j4$DateTime, format="%Y-%m-%d %H:%M:%S")

j1_test$DateTime <- strptime(j1_test$DateTime, format="%Y-%m-%d %H:%M:%S")
j2_test$DateTime <- strptime(j2_test$DateTime, format="%Y-%m-%d %H:%M:%S")
j3_test$DateTime <- strptime(j3_test$DateTime, format="%Y-%m-%d %H:%M:%S")
j4_test$DateTime <- strptime(j4_test$DateTime, format="%Y-%m-%d %H:%M:%S")

#####EDA#####

plot_vehicles <- function(j, title=""){
  plot(j$DateTime, j$Vehicles, type = "l", xaxt = "n", xlab="Date", 
       ylab="# of Vehicles", main=title)
  axis.POSIXct(1, at=seq.POSIXt(j$DateTime[1], j$DateTime[length(j$DateTime)], by = "month"), format="%Y-%m")
}

#plot time series for each junction
# par(mfrow=c(2,2)) #uncomment this line to plot all 4 in same plot
plot_vehicles(j1, "Junction 1")
plot_vehicles(j2, "Junction 2")
plot_vehicles(j3, "Junction 3")
plot_vehicles(j4, "Junction 4")
#par(mfrow=c(1,1)) #switch back to normal plotting if needed

head(j1$DateTime)
head(j2$DateTime)
head(j3$DateTime)
head(j4$DateTime)
#junctions 1-3 all start at the same date and time in 2015, but junction 4
#doesn't start until 2017
#this means that junction 4 was either created and opened on 2017-01-01, or 
#they only started tracking data for junction 4 on that date, we will probably 
#have to model this date as an intervention point

tail(j1$DateTime)
tail(j2$DateTime)
tail(j3$DateTime)
tail(j4$DateTime)
#all 4 junctions end at the same time

#j1-3 all have the same number of observations so we can just add them up
total <- j1$Vehicles + j2$Vehicles + j3$Vehicles

ts.plot(total)
#find out where we have to subset from to add j4
j4_start_idx <- length(total) - length(j4$Vehicles) + 1

#double check this corresponds to 2017-01-01 for the other junctions
j1$DateTime[j4_start_idx]
total[j4_start_idx:length(total)] <- total[j4_start_idx:length(total)] +j4$Vehicles
ts.plot(total)

#there is only a slight difference in the graph because junction 4 does not have
#very many vehicles

traffic_total <- data.frame(DateTime=j1$DateTime, Vehicles=total, ID=j1$ID)
ts.plot(traffic_total$Vehicles)
plot_vehicles(traffic_total, "All junctions")

#function to run stationarity tests and plot acf, pacf
stationarity <- function(ts){
  print(kpss.test(ts$Vehicles, null = "T"))
  print(adf.test(ts$Vehicles))
  par(mfrow=c(1,2))
  acf(ts$Vehicles)
  pacf(ts$Vehicles)
  par(mfrow=c(1,1))
}

stationarity(j1)
stationarity(j2)
stationarity(j3)
stationarity(j4)
stationarity(traffic_total)

library("forecast")
library("xts")
library("TSA")
library("MTS")

#periodogram to identify seasonality
p <- periodogram(traffic_total$Vehicles)

#looks like most important frequencies are above 0.75e6
high_freq <- p$freq[which(p$spec > 0.75e6)]
seasonality <- 1 / high_freq
seasonality

#we see unsurprising seasonalities at 7 days, 24 hours, and 12 hours
#but also at 15000, 7500, 5000 hours which isn't clear as to why

msts_data <- msts(traffic_total$Vehicles, seasonal.periods = c(168, 24, 12))
tbats_model <- tbats(msts_data)
comp <- tbats.components(tbats_model)
plot(comp)
plot(forecast(tbats_model, h=100))

#####import libraries#####
library(tseries)
library(forecast)
library(Metrics)
library("xts")
library("TSA")
library("MTS")

#####load data######
#train data
traffic <- read.csv("/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/train_ML_IOT.csv")
j1 <- traffic[traffic$Junction == 1,]
j2 <- traffic[traffic$Junction == 2,]
j3 <- traffic[traffic$Junction == 3,]
j4 <- traffic[traffic$Junction == 4,]

#test data
traffic_test <- read.csv("/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/test_ML_IOT.csv")

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

#train test splits using last 30 days as testing set
l <- length(traffic_total$Vehicles)
test_length <- 30 * 24 #30 days times 24 hours
train_end <- (l - test_length)
test_start <- (l - test_length + 1)

traffic_train_split <- traffic_total[1:train_end,]
traffic_test_split <- traffic_total[test_start:l,]

j1_train_split <- j1[1:train_end,]
j1_test_split <- j1[test_start:l,]

j2_train_split <- j2[1:train_end,]
j2_test_split <- j2[test_start:l,]

j3_train_split <- j3[1:train_end,]
j3_test_split <- j3[test_start:l,]

j4_train_split <- j4[1:train_end,]
j4_test_split <- j4[test_start:l,]

#periodogram to identify seasonality
p <- periodogram(traffic_total$Vehicles)

#looks like most important frequencies are above 0.75e6
high_freq <- p$freq[which(p$spec > 0.75e6)]
seasonality <- 1 / high_freq
seasonality

#we see unsurprising seasonalities at 7 days, 24 hours, and 12 hours
#but also at 15000, 7500, 5000 hours which isn't clear as to why
#with many seasonalities and most of them not at small integer values,
#tbats may be a good candidate model (De Livera et al., 2010)

#####TBATS MODEL#####
#first let's try a TBATS model with all 6 seasonalities that we identified
msts_data <- msts(traffic_train_split$Vehicles, seasonal.periods = seasonality)
tbats_model <- tbats(msts_data)
comp <- tbats.components(tbats_model)
plot(comp)
tbats_fc <- forecast(tbats_model, h=720)
plot(tbats_fc)
smape(traffic_test_split$Vehicles, tbats_fc$mean)
#SMAPE is 0.184

#what if we only use the easily explainable seasonalities?
msts_data2 <- msts(traffic_train_split$Vehicles, seasonal.periods = c(168, 24, 12))
tbats_model2 <- tbats(msts_data2)
comp2 <- tbats.components(tbats_model2)
plot(comp2)
tbats_fc2 <- forecast(tbats_model2, h=720)
plot(tbats_fc2)
smape(traffic_test_split$Vehicles, tbats_fc2$mean)
#SMAPE improves to 0.116

#now let's try tbats on individual junctions and submit to kaggle

#function to return tbats forecast for a given junction
#default params are to forecast for test set to submit to kaggle
junction_tbats <- function(j, fc_h=2952, seasonality=c(168,24,12)){
  j_tbats <- tbats(msts(j$Vehicles, seasonal.periods = seasonality))
  fc <- forecast(j_tbats, h=fc_h)
  plot(fc)
  return(fc)
}

j1_tbats_fc <- junction_tbats(j1)
j2_tbats_fc <- junction_tbats(j2)
j3_tbats_fc <- junction_tbats(j3)
j4_tbats_fc <- junction_tbats(j4)

junction_fcs <- c(j1_tbats_fc$mean, j2_tbats_fc$mean, 
                  j3_tbats_fc$mean, j4_tbats_fc$mean)
sample <- read.csv("traffic/sample_submission_ML_IOT.csv")
submission <- data.frame(sample$ID, junction_fcs)
colnames(submission) <- c('ID', 'Vehicles')
write.csv(submission, "tbats_submission.csv", row.names=FALSE)
#submission is 65th place

###################


####### Auto Arima and STL  MODELS ##########
#auto.arima
msts_data <- msts(traffic_train_split$Vehicles, seasonal.periods = seasonality)
ts.plot(msts_data)
auto.arima.m <- auto.arima(msts_data)
summary(auto.arima.m)
checkresiduals(auto.arima.m)
arima_fore <- forecast(stl_model, h=720)
smape(traffic_test_split$Vehicles, arima_fore$mean)
plot(arima_fore)
#smape is .3521546 




## STL Model 
msts_data <- msts(traffic_train_split$Vehicles, seasonal.periods = seasonality, ts.frequency = 500)
s.p <- stl(msts_data, s.window = "periodic")
summary(s.p)
stl_model_forecast <- forecast(s.p, h=720)
smape(traffic_test_split$Vehicles, stl_model_forecast$mean)
plot(s.p)
plot(forecast(s.p, h = 720))

##testing each model to kaggle for arima 
#function to return arima forecast for a given junction
#default params are to forecast for test set to submit to kaggle
junction_arima <- function(j, fc_h=2952, seasonality=c(168,24,12)){
  j_arima <- auto.arima(msts(j$Vehicles, seasonal.periods = seasonality))
  fc <- forecast(j_arima, h=fc_h)
  plot(fc)
  return(fc)
}

j1_arima_fc <- junction_arima(j1)
j2_arima_fc <- junction_arima(j2)
j3_arima_fc <- junction_arima(j3)
j4_arima_fc <- junction_arima(j4)

junction_fcs_arima <- c(j1_arima_fc$mean, j2_arima_fc$mean, 
                  j3_arima_fc$mean, j4_arima_fc$mean)
sample <- read.csv("/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/sample_submission_ML_IOT.csv")
submission <- data.frame(sample$ID, junction_fcs_arima)
colnames(submission) <- c('ID', 'Vehicles')
write.csv(submission, "/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/arima_submission.csv", row.names=FALSE)
#rank 165 



##testing each model to kaggle ofr STL
#function to return STL forecast for a given junction
#default params are to forecast for test set to submit to kaggle
junction_stl <- function(j, fc_h=2952, seasonality=c(168,24,12)){
  j_stl <- stl(msts(j$Vehicles, seasonal.periods = seasonality, ts.frequency = 500), s.window = "periodic")
  fc <- forecast(j_stl, h=fc_h)
  plot(fc)
  return(fc)
}

j1_stl_fc <- junction_stl(j1)
j2_stl_fc <- junction_stl(j2)
j3_stl_fc <- junction_stl(j3)
j4_stl_fc <- junction_stl(j4)

junction_fcs_stl <- c(j1_stl_fc$mean, j2_stl_fc$mean, 
                        j3_stl_fc$mean, j4_stl_fc$mean)
sample <- read.csv("/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/sample_submission_ML_IOT.csv")
submission <- data.frame(sample$ID, junction_fcs_stl)
colnames(submission) <- c('ID', 'Vehicles')
write.csv(submission, "/Users/JosephTomal_1/Desktop/Time Series/traffic_forecasting-main/stl_submission.csv", row.names=FALSE)
#rank 167

###################
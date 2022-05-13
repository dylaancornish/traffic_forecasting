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

plot_junction <- function(j, title=""){
  plot(j$DateTime, j$Vehicles, type = "l", xaxt = "n", xlab="Date", 
       ylab="# of Vehicles", main=title)
  axis.POSIXct(1, at=seq.POSIXt(j1$DateTime[1], j1$DateTime[length(j1$DateTime)], by = "month"), format="%Y-%m")
}

#plot time series for each junction
# par(mfrow=c(2,2)) #uncomment this line to plot all 4 in same plot
plot_junction(j1, "Junction 1")
plot_junction(j2, "Junction 2")
plot_junction(j3, "Junction 3")
plot_junction(j4, "Junction 4")
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


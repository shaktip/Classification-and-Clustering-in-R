#For clustering, ignore the date and time features in the data. Also, identify and ignore the
#entries with missing values. Use R to develop the code for k means clustering. First, consider all the
#remaining features for clustering. Then, consider the last three features (7-9) only. In each case, vary
#the value of k starting from 5 to 20. Note clustering accuracy and execution time. Generate input file to
#display variation of accuracy and execution time with the value of k in Graphviz (for the two cases – all
#features and only the last three features).



# clear workspace
rm(list=ls())
# Clear Console
cat("\014")
#library(ggplot2)


###########################################################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
###########################################################33

MyData <- read.csv("household_power_consumption.txt", sep=";", strip.white=TRUE)
original <- MyData
View(original)

MyData <- na.omit(MyData)

#################################################
#1st Column
MyData$Date <- NULL
#2nd Column
MyData$Time <- NULL

#3 Column : 1
MyData$Global_active_power = NULL
#4 Column  : 2
MyData$Global_reactive_power = NULL
#5 Column  : 3
MyData$Voltage <- NULL
#6 Column  : 4
MyData$Global_intensity <- NULL
#7 Column  : 5
#MyData$Sub_metering_1 <- NULL
#8 Column  : 6
#MyData$Sub_metering_2 <- NULL
#9 Column : 7
#MyData$Sub_metering_3 <- NULL
#################################################
MyData <- data.matrix(MyData)
MyData <- scale(MyData)

set.seed(20)

#MyData <- na.action(MyData)
View(MyData)

accuracy <- c()
tm <- c()
kvalues <- c()

for(k in 5:20)  
{
  ptm <- Sys.time()
  DataSetCluster <- kmeans(MyData , centers = k)
  tm <- c(tm , Sys.time() - ptm)
  kvalues <- c(kvalues, k)
  
  
  DataSetCluster
  accuracy <- c ( accuracy , DataSetCluster$betweenss / DataSetCluster$totss);
  print(paste(" k is ", k))
  
}
print(kvalues)
print(tm)
print(accuracy)

#plot(x,y)

#To save into a file
png('Last3ParametersTime.png')
plot(kvalues , tm , type = "l", main = "K V/s Time For 3 Parameters", xlab = "K of K-Means" , ylab = "Time For Different No of K value")
dev.off()


png('Last3ParametersAccuracy.png')
plot(kvalues ,accuracy , type = "l", main = "K V/s Accuracy For 3 Parameters", xlab = "K of K-Means" , ylab = "Accuracy For Different No of K value")

dev.off()




#View(DataSetCluster)

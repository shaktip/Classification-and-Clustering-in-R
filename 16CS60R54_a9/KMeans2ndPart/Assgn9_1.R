
#Submit your R code which should take the value of k and the
#set of considered features (Using the attribute numbers mentioned in the data set) as input. It is OK to
#hardcode the data set name and parsing of the data in your program. For example, if we want to run
#your code using feature numbers 3-9 with k= 6, we will pass 6 and 3,4,5,6,7,8,9 as parameters. If we
#want to run your code using feature numbers 7-9 with k=10, we will pass 10 and 7,8,9 as parameters.
#[50 Marks]


# clear workspace
rm(list=ls())
# Clear Console
cat("\014")
#library(ggplot2)


###########################################################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

SetColumn <- function(MyData , v)
{
  
  for(i in 1: ncol(MyData))
  {
    if(i %in% v == FALSE)
    {  
      if(i == 1)
      {
        MyData$Date <- NULL
      }
      else if(i == 2)
      {
        MyData$Time <- NULL
      }
      else if(i == 3)
      {
        MyData$Global_active_power = NULL
      }
      else if(i==4)
      {
        MyData$Global_reactive_power = NULL
      }
      else if(i == 5)
      {
        MyData$Voltage <- NULL
      }
      else if(i == 6)
      {
        MyData$Global_intensity <- NULL
      }
      else if(i == 7)
      {
        MyData$Sub_metering_1 <- NULL
      }
      else if(i == 8)
      {
        MyData$Sub_metering_2 <- NULL
      }
      else if(i == 9)
      {
        MyData$Sub_metering_3 <- NULL
      }
    }
  }
  return(MyData)
}
###########################################################33

MyData <- read.csv("household_power_consumption.txt", sep=";", strip.white=TRUE)
original <- MyData
#View(original)

MyData <- na.omit(MyData)

#################################################
#1st Column
#MyData$Date <- NULL
#2nd Column
#MyData$Time <- NULL

#3 Column : 1
#MyData$Global_active_power = NULL
#4 Column  : 2
#MyData$Global_reactive_power = NULL
#5 Column  : 3
#MyData$Voltage <- NULL
#6 Column  : 4
#MyData$Global_intensity <- NULL
#7 Column  : 5
#MyData$Sub_metering_1 <- NULL
#8 Column  : 6
#MyData$Sub_metering_2 <- NULL
#9 Column : 7
#MyData$Sub_metering_3 <- NULL
#################################################

set.seed(20)

#MyData <- na.action(MyData)
View(MyData)

k <- as.numeric(readline("Enter k (positive integer): "))
ColValues <- readline("Enter Required Columns seperated by comma: ")
v <- as.list(strsplit(ColValues, ",")[[1]])
print(v)
#v <- c(3,4,5)
MyData <- SetColumn(MyData , v)
View(MyData)


DataSetCluster <- kmeans(MyData , centers = k)
DataSetCluster
accuracy <- DataSetCluster$betweenss / DataSetCluster$totss;
print(paste("Accuracy is ", accuracy))

#View(DataSetCluster)


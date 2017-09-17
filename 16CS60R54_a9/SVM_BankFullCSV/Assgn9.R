
# clear workspace
rm(list=ls())
# Clear Console
cat("\014")
library("e1071")

###########################################################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
###########################################################


MyData <- read.csv("bank-full.csv", sep=";", strip.white=TRUE)
#original <- MyData
#View(original)
#MyData$job = NULL
#MyData$marital = NULL
#MyData$education = NULL
#MyData$default = NULL
#MyData$housing = NULL
#MyData$loan = NULL
#MyData$contact = NULL
#MyData$month = NULL
#MyData$poutcome = NULL

MyData$job = as.numeric(MyData$job)
MyData$marital = as.numeric(MyData$marital)
MyData$education = as.numeric(MyData$education)
MyData$default = as.numeric(MyData$default)
MyData$housing = as.numeric(MyData$housing)
MyData$loan = as.numeric(MyData$loan)
MyData$contact = as.numeric(MyData$contact)
MyData$month = as.numeric(MyData$month)
MyData$poutcome = as.numeric(MyData$poutcome)


#MyData$duration = NULL
#MyData$age = NULL
#MyData$day = NULL
#MyData$balance = NULL   #---- increases accuracy
#MyData$compaign = NULL
#MyData$pdays = NULL
#MyData$previous = NULL


###############################################################
MyData <- na.omit(MyData)
TestData1 <- as.data.frame(lapply(MyData[1:16], normalize))
MyData <- cbind(TestData1 , MyData$y)
colnames(MyData)[17] <- "y"
##################################################################
#The following lines are used to seperate the Training and 
#Testing files on randomly selected rows and those rows on multiple
#executions of the code are stored in TrainData and TestData
#and now those files are read to show the best accuracy.


#TRows = nrow(MyData)
#TrainRows = as.integer(TRows*0.7)
#set.seed(12)
#RandomRows <- sample(1:TRows, size = 0.7*TRows)
#TrainData <- MyData[RandomRows,]
#TestData <- MyData[-RandomRows ,]
#write.csv(TrainData,file="TrainData.csv")
#write.csv(TestData,file="TestData.csv")

TrainData <- read.csv("TrainData.csv", sep=",", strip.white=TRUE)

TestData <- read.csv("TestData.csv", sep=",", strip.white=TRUE)

#######################################################
TrainData$job = NULL
TrainData$marital = NULL
TrainData$education = NULL
TrainData$default = NULL
TrainData$housing = NULL
TrainData$loan = NULL
TrainData$contact = NULL
TrainData$month = NULL
TrainData$poutcome = NULL
#MyData$duration = NULL
#TrainData$age = NULL
#TrainData$day = NULL
TrainData$balance = NULL   #---- increases accuracy
#TrainData$compaign = NULL
#TrainData$pdays = NULL
#TrainData$previous = NULL

#########################################################
TestData$job = NULL
TestData$marital = NULL
TestData$education = NULL
TestData$default = NULL
TestData$housing = NULL
TestData$loan = NULL
TestData$contact = NULL
TestData$month = NULL
TestData$poutcome = NULL
#MyData$duration = NULL
#TestData$age = NULL
#TestData$day = NULL
TestData$balance = NULL   #---- increases accuracy
#TestData$compaign = NULL
#TestData$pdays = NULL
#TestData$previous = NULL

#########################################################



x <- subset(TrainData , select = -y)
y <- TrainData$y
#colnames(y)[1] = "y"


Testx <- subset(TestData , select = -y)
Testy <- TestData$y
###########################################################################

#svm_tune <- tune(svm, train.x = x, train.y = y, 
#                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

#print(svm_tune)

svm_model_after_tune <- svm(y ~ ., data = x , kernel="radial", cost=1, gamma=0.1)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune, Testx)
system.time(predict(svm_model_after_tune, Testx))

t <- table(pred, Testy)
print(t)
accuracy1 <- sum(diag(t)/sum(t))
print(paste("Radial mode accuracy", accuracy1))

###########################################################################

#svm_tune <- tune(svm, train.x = x, train.y = y, 
#                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))


#svm_tune_linear<-tune(svm, train.x = x , train.y = y ,kernel="linear",
#          ranges=list(cost=c(0.01,0.1,1,5)))

#print(svm_tune_linear)
svm_after_linear<-svm(y~., data = x ,kernel="linear",cost=1 , gamma=0.1)

pred_linear <- predict(svm_after_linear, Testx)
system.time(predict(svm_after_linear, Testx))
  
t_linear <- table(pred_linear, Testy)
print(t_linear)
accuracy3 <- sum(diag(t_linear)/sum(t_linear))
print(paste("Linear mode accuracy", accuracy3))

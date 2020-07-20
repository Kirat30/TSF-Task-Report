#TASK1
studied <- read.csv("tsk1.csv")
modFit <- lm(Scores~Hours,data = studied)
plot(x = studied$Hours,y = studied$Scores,main = "Scores Vs Hours Studied",xlab = "Hours Studied",ylab = "Exam Scores",col = "red")
abline(modFit, col="blue")
nwdat <- data.frame(Hours = 9.5)
predict(modFit,newdata = nwdat)

#TASK2
library(caret)
library(rattle)
irisdata <- read.csv("iris.csv")
irisdata <- irisdata[,-1]
irisdata$Species <- gsub(pattern = "Iris-",replacement = "",x = irisdata$Species,useBytes = TRUE)
set.seed(72020)
inTrain <- createDataPartition(irisdata$Species,p = 0.7,list = FALSE)
training <- irisdata[inTrain,]
testing <- irisdata[-inTrain,]
testing$Species <- as.factor(testing$Species)
treeFit <- train(Species~.,data = training,method = "rpart")
fancyRpartPlot(treeFit$finalModel)
pr <- predict(treeFit,testing)
cmat <- confusionMatrix(pr,testing$Species)
cmat$overall[1]
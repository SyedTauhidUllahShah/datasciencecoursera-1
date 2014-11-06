# Quiz 2
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(ggplot2)


# Question 1
data(AlzheimerDisease)

adData1 = data.frame(diagnosis,predictors)
trainIndex1 = createDataPartition(diagnosis,p=0.5,list=FALSE)
training1 = adData1[trainIndex1,]
testing1 = adData1[-trainIndex1,]

# Question 2
data(concrete)
set.seed(975)
mixtures[,1:8] <- as.data.frame( lapply(mixtures[,1:8], cut2, g=10, levels.mean = TRUE) )
inTrain2 = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training2 = mixtures[ inTrain2,]
testing2 = mixtures[-inTrain2,]
xTrain2 = seq(1,length(training2$CompressiveStrength))
xTest2 = seq(1,length(testing2$CompressiveStrength))
str(training2)

qplot(xTrain2, CompressiveStrength, color = Cement, data = training2)
qplot(xTrain2, CompressiveStrength, color = BlastFurnaceSlag, data = training2)
qplot(xTrain2, CompressiveStrength, color = FlyAsh, data = training2)
qplot(xTrain2, CompressiveStrength, color = Water, data = training2)
qplot(xTrain2, CompressiveStrength, color = Superplasticizer, data = training2)
qplot(xTrain2, CompressiveStrength, color = CoarseAggregate, data = training2)
qplot(xTrain2, CompressiveStrength, color = FineAggregate, data = training2)
qplot(xTrain2, CompressiveStrength, color = Age, data = training2)

# Question 3
data(concrete)
set.seed(975)
inTrain3 = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training3 = mixtures[ inTrain3,]
testing3 = mixtures[-inTrain3,]
hist(training3$Superplasticizer)
#plot(log10(training3$Superplasticizer))

# Question 4
set.seed(3433)
data(AlzheimerDisease)
adData4 = data.frame(diagnosis,predictors)
inTrain4 = createDataPartition(adData4$diagnosis, p = 3/4)[[1]]
training4 = adData4[ inTrain4,]
testing4 = adData4[-inTrain4,]

PredIL4 <- grep("^IL", names(training4))
preProc4 <- preProcess(training4[,PredIL4], method="pca", thresh=.8)
print(preProc4$numComp)

# Question 5
set.seed(3433)
data(AlzheimerDisease)
adData5 = data.frame(diagnosis,predictors)
trainingIndex5 <- grep("^IL", names(adData5))
inTrain5 = createDataPartition(adData5$diagnosis, p = 3/4)[[1]]
training5 = cbind(adData5[ inTrain5,trainingIndex5], adData5[ inTrain5,"diagnosis"])
testing5 = cbind(adData5[ -inTrain5,trainingIndex5], adData5[ -inTrain5,"diagnosis"])
colnames(training5)[13] <- "diagnosis"
colnames(testing5)[13] <- "diagnosis"

preProc5 <- preProcess(training5[,-13], method="pca", thresh=.8)
trainPC5 <- predict(preProc5, training5[,-13])
modelFitPC5 <- train(training5$diagnosis ~ ., method="glm", data=trainPC5)
testPC5 <- predict(preProc5, testing5[,-13])
print(confusionMatrix(testing5$diagnosis, predict(modelFitPC5, testPC5)))

modelFitFull5 <- train(training5$diagnosis ~ ., method="glm", data=training5)
print(confusionMatrix(testing5$diagnosis, predict(modelFitFull5, testing5)))

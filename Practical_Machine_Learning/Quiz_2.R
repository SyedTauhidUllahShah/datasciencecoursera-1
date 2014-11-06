# Quiz 2
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(ggplot2)


# Question 1
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2
data(concrete)
set.seed(975)
mixtures[,1:8] <- as.data.frame( lapply(mixtures[,1:8], cut2, g=10, levels.mean = TRUE) )
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
xTrain = seq(1,length(training$CompressiveStrength))
xTest = seq(1,length(testing$CompressiveStrength))
str(training)

qplot(xTrain, CompressiveStrength, color = Cement, data = training)
qplot(xTrain, CompressiveStrength, color = BlastFurnaceSlag, data = training)
qplot(xTrain, CompressiveStrength, color = FlyAsh, data = training)
qplot(xTrain, CompressiveStrength, color = Water, data = training)
qplot(xTrain, CompressiveStrength, color = Superplasticizer, data = training)
qplot(xTrain, CompressiveStrength, color = CoarseAggregate, data = training)
qplot(xTrain, CompressiveStrength, color = FineAggregate, data = training)
qplot(xTrain, CompressiveStrength, color = Age, data = training)

# Question 3
data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
#plot(log10(training$Superplasticizer))

# Question 4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

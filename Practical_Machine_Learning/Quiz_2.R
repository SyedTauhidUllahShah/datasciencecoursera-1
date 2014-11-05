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

colors <- names(mixtures)
colors <- colors[-9]
g <- ggplot(training, aes(x = xTrain, y = CompressiveStrength, color = FlyAsh))
g <- g + geom_point()
print(g)
# Quiz 3

library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)

# Question 1
data(segmentationOriginal)
set.seed(125)
training1 = segmentationOriginal[segmentationOriginal$Case=="Train",]
testing1 = segmentationOriginal[segmentationOriginal$Case=="Test",]
modFit1 <- train(Class ~ ., method="rpart", data=training1)
print(modFit1$finalModel)

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]
inTrain3 = createDataPartition(olive$Area, p = 3/4)[[1]]
training3 = olive[ inTrain3,]
testing3 = olive[-inTrain3,]
modFit3 <- train(Area ~ ., method="rpart", data=training3)
print(modFit3$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
print(predict(modFit3, newdata = newdata))

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit4 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                 method="glm", family="binomial", data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
trainPred <- predict(modFit4, trainSA[,-10])
testPred <- predict(modFit4, testSA[,-10])
print(missClass(testSA$chd,testPred))
print(missClass(trainSA$chd,trainPred))

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modfit5 <- train(y ~ ., method="rf", prox=TRUE, data=vowel.train)
varImp(modfit5)
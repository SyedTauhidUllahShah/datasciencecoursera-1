# Quiz 4
library(caret)

# Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modFitRF <- train(y ~ ., method="rf", prox=TRUE, data=vowel.train)
modFitBoost <- train(y ~ ., method="gbm", data=vowel.train, verbose=FALSE)
predRF = predict(modFitRF, vowel.test)
predBoost = predict(modFitBoost, vowel.test)

print(confusionMatrix(vowel.test$y, predRF))
print(confusionMatrix(vowel.test$y, predBoost))

agreementSub = vowel.test[predRF == predBoost, ]
predBoth = predict(modFitRF, agreementSub)
AgreeAccuracy = sum(predBoth==agreementSub$y)/length(predBoth)

# Question 2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData2 = data.frame(diagnosis,predictors)
inTrain2 = createDataPartition(adData2$diagnosis, p = 3/4)[[1]]
training2 = adData2[ inTrain2,]
testing2 = adData2[-inTrain2,]

set.seed(62433)
modFitRF2 = train(diagnosis ~ ., method="rf", prox=TRUE, data = training2)
modFitBoost2 = train(diagnosis ~ ., method="gbm", verbose=FALSE, data = training2)
modFitLDA2 = train(diagnosis ~ ., method="lda", data = training2)
predRF2 = predict(modFitRF2, testing2)
predBoost2 = predict(modFitBoost2, testing2)
predLDA2 = predict(modFitLDA2, testing2)

print(confusionMatrix(testing2$diagnosis, predRF2))
print(confusionMatrix(testing2$diagnosis, predBoost2))
print(confusionMatrix(testing2$diagnosis, predLDA2))

dfPredComb = data.frame(predRF2, predBoost2, predLDA2, diagnosis=testing2$diagnosis)
modFitComb = train(diagnosis ~ ., method="rf", data=dfPredComb, prox=TRUE)
predComb = predict(modFitComb, dfPredComb)
print(confusionMatrix(testing2$diagnosis, predComb))

# Question 3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain3 = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training3 = concrete[ inTrain3,]
testing3 = concrete[-inTrain3,]

set.seed(233)
modFit3 = train(CompressiveStrength ~ ., method="lasso", data=training3)
plot(modFit3$finalModel, xvar="penalty", use.color=TRUE)

# Question 4
library(lubridate)  # For year() function below
library(forecast)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

fit4 <- bats(tstrain)
fcast <- forecast(fit4, h=length(testing$X))
#fcast <- forecast(fit4)
testing <- testing[1:length(fcast$lower[,2]),]
testCompare <- sum((testing$visitsTumblr >= fcast$lower[,2])&(testing$visitsTumblr <= fcast$upper[,2]))
percentCI = testCompare/length(testing$X)

# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
model <- svm(CompressiveStrength ~ ., data=training )
testPred <- predict(model, testing)
RMSE = sqrt(mean((testPred - testing$CompressiveStrength)^2))

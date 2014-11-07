# Quiz 4

# Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modFitRF <- train(y ~ ., method="rf", prox=TRUE, data=vowel.train)
modFitBoost <- train(y ~ ., method="gbm", data=vowel.train, verbose=FALSE)
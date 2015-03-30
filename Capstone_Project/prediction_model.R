# Code to generate language prediction model

# 1. Preprocess the data
#source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/Regression_Model_PreProcessing.R')

# 2. run a lit model fit
fit <- lm(Y ~. + .^4, data <- TknRegGrams)

#3. Predict a word after a 4-gram
FourGramX <- as.data.frame(c("who", "can","it","be"))
FourGramX[,1] <- as.character(FourGramX[,1])
FourGramX <- GramsToInts(FourGramX,TknRegWordsDict$words, TknRegWordsDict$ID)
Xdf <- data.frame()
for (i in 1:length(FourGramX)){
     Xdf[1,i] = FourGramX[i]
}

colnames(Xdf) <- c("W1", "W2", "W3", "W4")
Pred <- predict(fit, newdata = Xdf, type = "response")
PredWord <- TknRegWordsDict$words[Pred]

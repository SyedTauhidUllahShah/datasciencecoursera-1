# Code to generate language prediction model

# 1. Preprocess the data
#source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/Regression_Model_PreProcessing.R')
load('TknRegGrams.RData')
load('TknRegWordsDict.RData')


# 2. Divide the data into training and test sets
set.seed(12321)
smpLen <- nrow(TknRegGrams)
smpPer <- ceiling(.70*smpLen)
train <- sample(1:smpLen, size = smpPer)

# 3. run a boost model fit with cross-validation
require(gbm, quietly = TRUE, warn.conflicts = FALSE)
n.trees = seq(from = 600, to = 3000, by = 100)
interaction.depth = c(2, 3, 4)
shrinkage = c(0.001, 0.01, 0.1)

totalRuns <- length(n.trees) * length(interaction.depth) * length(shrinkage)
predMat = matrix(0, nrow = totalRuns, ncol = 4)
runNum = 0

for(j in seq_along(interaction.depth)){
     for(k in seq_along(shrinkage)){
          runNum = runNum + 1
          print(paste0('Run number ',runNum, ' out of ', totalRuns))
          boost.fit <- gbm(Y ~ .,
                           data = TknRegGrams[train,],
                           distribution = "gaussian",
                           n.trees = 3000,
                           shrinkage = shrinkage[k],
                           interaction.depth = interaction.depth[j],
                           n.cores = 6,
                           keep.data = FALSE)
          save(boost.fit, file=paste0('boost-3000-',interaction.depth[j],
                                      '-',shrinkage[k],'.model'))
     }
}

runNum = 0
for(i in seq_along(n.trees)){
     for(j in seq_along(interaction.depth)){
          for(k in seq_along(shrinkage)){
               runNum = runNum + 1
               print(paste0('Run number ',runNum, ' out of ', totalRuns))
               rm(boost.fit)
               load(paste0('boost-3000-',interaction.depth[j],
                           '-',shrinkage[k],'.model'))
               pred = predict(boost.fit,
                              newdata=TknRegGrams[-train,],
                              n.trees = n.trees[i])
               predMat[runNum, 1] = with(TknRegGrams[-train,],
                                       mean((pred - Y)^2))
               predMat[runNum, 2] = n.trees[i]
               predMat[runNum, 3] = interaction.depth[j]
               predMat[runNum, 4] = shrinkage[k]
               
               print(paste0('Mean Square Prediction Error: ', predMat[runNum,1]))
               save(predMat, file='predMat.RData')
          }
     }
}

minRow <- which.min(predMat[,1])
bestNumTrees = predMat[minRow, 2]
bestIntDp = predMat[minRow, 3]
bestShrinkage = predMat[minRow, 4]

boost.fit <- gbm(Y ~ .,
                 data = TknRegGrams,
                 distribution = "gaussian",
                 n.trees = bestNumTrees,
                 shrinkage = bestShrinkage,
                 interaction.depth = bestIntDp,
                 n.cores = 6,
                 keep.data = FALSE)
save(boost.fit, file="boost.model")
#fit <- lm(Y ~. + .^4, data <- TknRegGrams)

#4. Predict a word after a 4-gram
convGramToDf <- function(gramVec, fromVec, toVec){
     # gramVec is a vector of characters
     n <- length(gramVec)
     numMissCols <- 4-n
     gramVec <- as.data.frame(gramVec, stringsAsFactors = FALSE)
     gramVec <- GramsToInts(gramVec, fromVec, toVec)
     gramVec <- as.data.frame(t(gramVec))
     columnNames <- c("V1", "V2", "V3", "V4")
     if(numMissCols > 0){
          gramVec[,(4-(numMissCols-1)):4]=0
          colnames(gramVec)[(4-(numMissCols-1)):4] = columnNames[(4-(numMissCols-1)):4]
     }
     return(gramVec)
}

testVec <- c("who", "can", "it", "be")
Xdf <- convGramToDf(testVec, TknRegWordsDict$words, TknRegWordsDict$ID)

#Pred <- as.integer(predict(fit, newdata = Xdf, type = "response"))
Pred <- as.integer(predict(boost.fit, newdata = Xdf, n.trees = bestNumTrees))
PredWord <- TknRegWordsDict$words[Pred]

library(shiny)

# Increase file upload size from 5MB to 30MB
options(shiny.maxRequestSize=30*1024^2)

#load('TknRegGrams.RData')
load('TknRegWordsDict.RData')
load('boost.model')
n.trees = 5000 

# Function to convert ngrams into dataframe with column for each word
ngramToMatrix <- function(dataVec, fromVec, toVec){
     require(plyr, quietly = TRUE, warn.conflicts = FALSE)
     # assume that data is a character vector
     line <- character()
     for (i in 1:length(dataVec)){
          line[i] <- strsplit(dataVec[i], split = " ")   
     }
     tempMat <- do.call("rbind", line)
     result = matrix(nrow=nrow(tempMat), ncol=ncol(tempMat))
     for (j in 1:ncol(tempMat)) {
          result[,j] <- mapvalues(tempMat[,j], from = fromVec,
                                  to=toVec, warn_missing=FALSE)
     }
     result <- apply(result, 2, as.integer)
     return(result)    
}

GramsToInts <- function(df, fromVec, toVec) {
     # Take the n-gram words and convert to integer matrix
     # as the last column
     result <- ngramToMatrix(df[,1], fromVec, toVec)
     result[is.na(result)==1] = 0
     #result <- result[rowSums(is.na(result))==0,]
     return(result)
}

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

predWord <- function(phrase, modelFit, wordsVec, IDVec) {
  require(gbm, quietly = TRUE, warn.conflicts = FALSE)
  phraseVec <- unlist(strsplit(phrase, split = " "))
  n <- length(phraseVec)
  if(n>4) {
    phraseVec <- phraseVec[(n-3):n]
  }
  Xdf <- convGramToDf(phraseVec, wordsVec, IDVec)
  Pred <- as.integer(predict(modelFit, newdata = Xdf, n.trees = n.trees))
  PredWord <- wordsVec[Pred]
  return(PredWord)
}

shinyServer(
     function(input, output)  {
          output$answer <- renderText({
               input$goButton
               isolate(predWord(input$txtInput, boost.fit, TknRegWordsDict$words, TknRegWordsDict$ID))
          })
     }
)
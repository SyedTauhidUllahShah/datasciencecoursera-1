# Regression model pre-processing

# 4GB RAM for Java
options(java.parameters = "-Xmx4g")

projDir <- '~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/'
setwd(projDir)
source(paste0(projDir,'clean_and_filter_corpus.R'))
source(paste0(projDir,'Katz_back_off.R'))
source(paste0(projDir,'tokenize_file.R'))
fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
US.path <- paste0(fileDir,"sampled/en_US.txt")

require(plyr, quietly = TRUE, warn.conflicts = FALSE)
require(RWeka, quietly = TRUE, warn.conflicts = FALSE)
require(tm, quietly = TRUE, warn.conflicts = FALSE)
require(SnowballC, warn.conflicts = FALSE, quietly = TRUE)

# Read in the data and split into pieces
cleanSplit <- readLinesFile(US.path)
cleanSplit <- split(cleanSplit, cut(1:length(cleanSplit), 10, labels=FALSE))

# Clean and filter the data
cleanedData <- lapply(cleanSplit, function(x) clean_and_filter_corpus(x))
rm(cleanSplit)

# Functions for 2-gram through 5-gram tokenizers
TriGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
QuadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
WordTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
QuintGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

PreProcessWordsDict <- function(data) {
     # Create words     
     words <- WordTokenizer(data)
     # create uniques of each n-gram
     uniqueWords <- unique(words)
     return(uniqueWords)
}

PreProcessGrams <- function(data, ngram) {   
     if(ngram==2){     
          TwoGrams <- BiGramTokenizer(data)
          Grams <- unique(TwoGrams)
     }
     if(ngram==3){     
          ThreeGrams <- TriGramTokenizer(data)
          Grams <- unique(ThreeGrams)
     }
     if(ngram==4){     
          FourGrams <- QuadGramTokenizer(data)
          Grams <- unique(FourGrams)
     }
     if(ngram==5){     
          FiveGrams <- QuintGramTokenizer(data)
          Grams <- unique(FiveGrams)
     }
     return(Grams)   
}

GramsToInts <- function(df, fromVec, toVec) {
     # Take the n-gram words and convert to integer matrix
     # as the last column
     result <- ngramToMatrix(df[,1], fromVec, toVec)
     result[is.na(result)==1] = 0
     #result <- result[rowSums(is.na(result))==0,]
     return(result)
}

AddZeroCols <- function(mat, largestNGram = 5) {
     nc <- ncol(mat)
     nr <- nrow(mat)

     if(nc==largestNGram){
          return(mat)
     }
     else {
          zeroMat <- mat.or.vec(nr = nr, nc = largestNGram-nc)
          if(nc==(largestNGram-1)){
               zeroMat <- as.integer(zeroMat) 
          }
          else {
               zeroMat <- apply(zeroMat, 2, as.integer)
          }
          # add zero columns to fill in matrix
          mat <- cbind(mat, zeroMat)
          # set the last column equal to the original last column
          mat[,ncol(mat)] = mat[,nc]
          # set the original last column to zeros
          mat[,nc] <- 0
          mat <- apply(mat, 2, as.integer)
          return(mat)        
     }
}
##################  Begin preprocessing

# Combine words dictionary
TknRegWordsDict <- lapply(cleanedData, PreProcessWordsDict)
TknRegWordsDict <- unlist(TknRegWordsDict)
TknRegWordsDict <- as.data.frame(unique(TknRegWordsDict),
                                 stringsAsFactors = FALSE)
colnames(TknRegWordsDict)[1] <- "words"

# Add row index to the uniqueWords list
TknRegWordsDict$ID <- as.integer(1:nrow(TknRegWordsDict))

# Create unique bigrams list
TknRegBiGrams <- lapply(cleanedData, function(x) PreProcessGrams(x,2))
TknRegBiGrams <- unlist(TknRegBiGrams)
TknRegBiGrams <- unique(TknRegBiGrams)
TknRegBiGrams <- as.data.frame(TknRegBiGrams, stringsAsFactors = FALSE)

# Create unique trigrams list
TknRegTriGrams <- lapply(cleanedData, function(x) PreProcessGrams(x,3))
TknRegTriGrams <- unlist(TknRegTriGrams)
TknRegTriGrams <- unique(TknRegTriGrams)
TknRegTriGrams <- as.data.frame(TknRegTriGrams, stringsAsFactors = FALSE)

# Create unique quadgrams list
TknRegQuadGrams <- lapply(cleanedData, function(x) PreProcessGrams(x,4))
TknRegQuadGrams <- unlist(TknRegQuadGrams)
TknRegQuadGrams <- unique(TknRegQuadGrams)
TknRegQuadGrams <- as.data.frame(TknRegQuadGrams, stringsAsFactors = FALSE)

# Create unique quintgrams list
TknRegQuintGrams <- lapply(cleanedData, function(x) PreProcessGrams(x,5))
TknRegQuintGrams <- unlist(TknRegQuintGrams)
TknRegQuintGrams <- unique(TknRegQuintGrams)
TknRegQuintGrams <- as.data.frame(TknRegQuintGrams, stringsAsFactors = FALSE)

rm(cleanedData)

# Convert n-grams to integer matrices
# First split each dataset into equal pieces
pieces <- 50
TknRegBiGramsSplit <- split(TknRegBiGrams, cut(1:nrow(TknRegBiGrams),
                                               pieces, labels=FALSE))
TknRegBiGramsInt <- lapply(TknRegBiGramsSplit, function(x) GramsToInts(x,TknRegWordsDict$words, TknRegWordsDict$ID))
rm(TknRegBiGramsSplit)
TknRegBiGramsInt <- lapply(TknRegBiGramsInt, function(x) AddZeroCols(x))
TknRegGrams <- do.call(rbind, TknRegBiGramsInt)
rm(TknRegBiGrams)
rm(TknRegBiGramsInt)

pieces <- 100
TknRegTriGramsSplit <- split(TknRegTriGrams, cut(1:nrow(TknRegTriGrams),
                                               pieces, labels=FALSE))
TknRegTriGramsInt <- lapply(TknRegTriGramsSplit, function(x) GramsToInts(x,TknRegWordsDict$words, TknRegWordsDict$ID))
rm(TknRegTriGramsSplit)
TknRegTriGramsInt <- lapply(TknRegTriGramsInt, function(x) AddZeroCols(x))
TknRegGramsMat <- do.call(rbind, TknRegTriGramsInt)
rm(TknRegTriGrams)
rm(TknRegTriGramsInt)
TknRegGrams <- rbind(TknRegGrams, TknRegGramsMat)
rm(TknRegGramsMat)

pieces <- 100
TknRegQuadGramsSplit <- split(TknRegQuadGrams, cut(1:nrow(TknRegQuadGrams),
                                                 pieces, labels=FALSE))
TknRegQuadGramsInt <- lapply(TknRegQuadGramsSplit, function(x) GramsToInts(x,TknRegWordsDict$words, TknRegWordsDict$ID))
rm(TknRegQuadGramsSplit)
TknRegQuadGramsInt <- lapply(TknRegQuadGramsInt, function(x) AddZeroCols(x))
TknRegGramsMat <- do.call(rbind, TknRegQuadGramsInt)
rm(TknRegQuadGrams)
rm(TknRegQuadGramsInt)
TknRegGrams <- rbind(TknRegGrams, TknRegGramsMat)
rm(TknRegGramsMat)

pieces <- 100
TknRegQuintGramsSplit <- split(TknRegQuintGrams, cut(1:nrow(TknRegQuintGrams),
                                                   pieces, labels=FALSE))
TknRegQuintGramsInt <- lapply(TknRegQuintGramsSplit, function(x) GramsToInts(x,TknRegWordsDict$words, TknRegWordsDict$ID))
rm(TknRegQuintGramsSplit)
TknRegQuintGramsInt <- lapply(TknRegQuintGramsInt, function(x) AddZeroCols(x))
TknRegGramsMat <- do.call(rbind, TknRegQuintGramsInt)
rm(TknRegQuintGrams)
rm(TknRegQuintGramsInt)
TknRegGrams <- rbind(TknRegGrams, TknRegGramsMat)
rm(TknRegGramsMat)

# Convert grams Matrix to data frame
TknRegGrams <- as.data.frame(TknRegGrams)
colnames(TknRegGrams) <- c("V1", "V2", "V3", "V4", "Y")

save(TknRegWordsDict, file = "TknRegWordsDict.RData")
save(TknRegGrams, file = "TknRegGrams.RData")
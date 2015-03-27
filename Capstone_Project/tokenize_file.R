# Function to tokenize and filter profanity form the text

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

GramWordsToInts <- function(df, fromVec, toVec) {
     # Take the n-gram words and convert to integer matrix with frequencies
     # as the last column
     encodeGram <- ngramToMatrix(df[,1], fromVec, toVec)
     result <- cbind(encodeGram, as.integer(df[,2]))
     result <- result[rowSums(is.na(result))==0,]
     return(result)
}

Tokenize <- function(data) {
     require(qdap, quietly = TRUE, warn.conflicts = FALSE)
     require(plyr, quietly = TRUE, warn.conflicts = FALSE)
     #require(SnowballC, warn.conflicts = FALSE, quietly = TRUE)
     #require(tm, quietly = TRUE, warn.conflicts = FALSE)
     #require(RWeka, quietly = TRUE, warn.conflicts = FALSE)
     
     # Functions for 2-gram through 4-gram tokenizers
#      TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#      BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#      QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
     
     # split the data by punctuation (except apostrophe) and unlist
     #words = unlist(strsplit(data, "(?!')[[:punct:]]|[[:space:]]", perl = T))
     #words = MC_tokenizer(data)
     
     # determine where the ngrams (up to n=4) lie
     z <- ngrams(data, n=4)
     grams <- unlist(z$raw)
     gramsList = sapply(gregexpr("\\w+", grams), length)
     
     words <- grams[gramsList == 1]
     words <- as.character(words)
     TwoGrams <- grams[gramsList == 2]
     TwoGrams <- as.character(TwoGrams)
     ThreeGrams <- grams[gramsList == 3]
     ThreeGrams <- as.character(ThreeGrams)
     FourGrams <- grams[gramsList == 4]
     FourGrams <- as.character(FourGrams)

     # Remove the original grams from memory
     rm(grams)
     rm(gramsList)

     # Number of tokens
     #tokens = length(words)
     numWords = length(words)
     numTwoGrams = length(TwoGrams)
     numThreeGrams = length(ThreeGrams)
     numFourGrams = length(FourGrams)

     # create data frame listing the number of occurrences of each word
     uniqueWords <- data.frame(table(words))
     uniqueWords$words <- as.character(uniqueWords$words)
     uniqueTwoGrams <- data.frame(table(TwoGrams))
     uniqueTwoGrams$TwoGrams <- as.character(uniqueTwoGrams$TwoGrams)
     uniqueThreeGrams <- data.frame(table(ThreeGrams))
     uniqueThreeGrams$ThreeGrams <- as.character(uniqueThreeGrams$ThreeGrams)
     uniqueFourGrams <- data.frame(table(FourGrams))
     uniqueFourGrams$FourGrams <- as.character(uniqueFourGrams$FourGrams)
     
     # Sort by frequency in descending order
     uniqueWords <- uniqueWords[order(uniqueWords[,2],decreasing=TRUE),]
     uniqueTwoGrams <- uniqueTwoGrams[order(uniqueTwoGrams[,2],decreasing=TRUE),]
     uniqueThreeGrams <- uniqueThreeGrams[order(uniqueThreeGrams[,2],decreasing=TRUE),]
     uniqueFourGrams <- uniqueFourGrams[order(uniqueFourGrams[,2],decreasing=TRUE),]

     # Add row index to the uniqueWords list
     uniqueWords$ID <- as.integer(1:nrow(uniqueWords))

     gramNames <- c("Words", "TwoGrams", "ThreeGrams", "FourGrams")
     gramVals <-c(numWords, numTwoGrams, numThreeGrams, numFourGrams)
     
     dfCounts <- data.frame()
     dfCounts <- data.frame(gramNames, gramVals)
     colnames(dfCounts) <- c("n.gram.type", "Total.Count")

     # Get Frequecy of Frequencies tables
     FreqOfFreqWords = ddply(uniqueWords, .(Freq), summarize,
                        Nr = length(Freq))
     FreqOfFreqTwoGrams = ddply(uniqueTwoGrams, .(Freq), summarize,
                           Nr = length(Freq))
     FreqOfFreqThreeGrams = ddply(uniqueThreeGrams, .(Freq), summarize,
                             Nr = length(Freq))
     FreqOfFreqFourGrams = ddply(uniqueFourGrams, .(Freq), summarize,
                            Nr = length(Freq))


     # Convert ngrams to matrices of integers
     rm(words)
     rm(TwoGrams)
     rm(ThreeGrams)
     rm(FourGrams)
     
     TGramMat <- GramWordsToInts(uniqueTwoGrams, uniqueWords$words,
                            uniqueWords$ID)
     uniqueTwoGrams <- TGramMat
     TGramMat <- GramWordsToInts(uniqueThreeGrams, uniqueWords$words,
                            uniqueWords$ID)
     uniqueThreeGrams <- TGramMat
     TGramMat <- GramWordsToInts(uniqueFourGrams, uniqueWords$words,
                            uniqueWords$ID)
     uniqueFourGrams <- TGramMat
     rm(TGramMat)

     resultList <- list("NGramCounts" = dfCounts,
                        "FreqOfFreqWords" = FreqOfFreqWords,
                        "FreqOfFreqTwoGrams" = FreqOfFreqTwoGrams,
                        "FreqOfFreqThreeGrams" = FreqOfFreqThreeGrams,
                        "FreqOfFreqFourGrams" = FreqOfFreqFourGrams,
                        "uniqueWords" = uniqueWords,
                        "uniqueTwoGrams" = uniqueTwoGrams,
                        "uniqueThreeGrams" = uniqueThreeGrams,
                        "uniqueFourGrams" = uniqueFourGrams)
     
     return(resultList)
}


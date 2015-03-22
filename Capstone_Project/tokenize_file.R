# Function to tokenize and filter profanity form the text

Tokenize <- function(data) {
     #require(qdap, quietly = TRUE, warn.conflicts = FALSE)
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
     TwoGrams <- grams[gramsList == 2]
     ThreeGrams <- grams[gramsList == 3]
     FourGrams <- grams[gramsList == 4]

     # Number of tokens
     #tokens = length(words)
     numWords = length(words)
     numTwoGrams = length(TwoGrams)
     numThreeGrams = length(ThreeGrams)
     numFourGrams = length(FourGrams)

     # create data frame listing the number of occurrences of each word
     #uniqueWords <- data.frame(table(words))
     uniqueWords <- data.frame(table(words))
     uniqueTwoGrams <- data.frame(table(TwoGrams))
     uniqueThreeGrams <- data.frame(table(ThreeGrams))
     uniqueFourGrams <- data.frame(table(FourGrams))
     
     gramNames <- c("Words", "TwoGrams", "ThreeGrams", "FourGrams")
     gramVals <-c(numWords, numTwoGrams, numThreeGrams, numFourGrams)
     
     dfCounts <- data.frame()
     dfCounts <- data.frame(gramNames, gramVals)
     colnames(dfCounts) <- c("n.gram.type", "Total.Count")
     
     resultList <- list("Words" = words, "TwoGrams" = TwoGrams,
                        "ThreeGrams" = ThreeGrams, "FourGrams" = FourGrams,
                        "NGramCounts" = dfCounts,
                        "uniqueWords" = uniqueWords,
                        "uniqueTwoGrams" = uniqueTwoGrams,
                        "uniqueThreeGrams" = uniqueThreeGrams,
                        "uniqueFourGrams" = uniqueFourGrams)
     
#      data <- VCorpus(VectorSource(data))
#      data <- tm_map(data, stripWhitespace)
#      dtm_uni <- DocumentTermMatrix(data)
#      dtm_bi <- DocumentTermMatrix(data, control = list(tokenize=BigramTokenizer))
#      dtm_tri <- DocumentTermMatrix(data, control = list(tokenize=TrigramTokenizer))
#      dtm_quad <- DocumentTermMatrix(data, control = list(tokenize=QuadgramTokenizer))
#      numWords <- sum(rowSums(as.matrix(dtm_uni)))
#      numTwoGrams <- sum(rowSums(as.matrix(dtm_bi)))
#      numThreeGrams <- sum(rowSums(as.matrix(dtm_tri)))
#      numFourGrams <- sum(rowSums(as.matrix(dtm_quad)))
# 
#      gramNames <- c("Words", "TwoGrams", "ThreeGrams", "FourGrams")
#      gramVals <-c(numWords, numTwoGrams, numThreeGrams, numFourGrams)
#      
#      dfCounts <- data.frame()
#      dfCounts <- data.frame(gramNames, gramVals)
#      colnames(dfCounts) <- c("N.Gram.Type", "Total.Count")
# 
#      resultList <- list("wordDTM" = dtm_uni,
#                         "twoGramDTM" = dtm_bi,
#                         "threeGramDTM" = dtm_tri,
#                         "fourGramDTM" = dtm_quad,
#                         "NGramCounts" = dfCounts)

     return(resultList)
}
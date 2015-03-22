# Function to tokenize and filter profanity form the text

Tokenize <- function(data) {
     require(qdap, quietly = TRUE, warn.conflicts = FALSE)
     require(SnowballC, warn.conflicts = FALSE, quietly = TRUE)
     require(tm, quietly = TRUE, warn.conflicts = FALSE)
     require(RWeka, quietly = TRUE, warn.conflicts = FALSE)
     
     # Functions for 2-gram through 4-gram tokenizers
     TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
     BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
     QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
     
     # split the data by punctuation (except apostrophe) and unlist
     #words = unlist(strsplit(data, "(?!')[[:punct:]]|[[:space:]]", perl = T))
     #words = MC_tokenizer(data)
     
#      # determine where the ngrams (up to n=4) lie
#      z <- ngrams(data, n=4)
#      grams <- unlist(z$raw)
#      gramsList = sapply(gregexpr("\\w+", grams), length)
#      
#      grams1 <- grams[gramsList == 1]
#      grams2 <- grams[gramsList == 2]
#      grams3 <- grams[gramsList == 3]
#      grams4 <- grams[gramsList == 4]
# 
#      # Number of tokens
#      #tokens = length(words)
#      num1grams = length(grams1)
#      num2grams = length(grams2)
#      num3grams = length(grams3)
#      num4grams = length(grams4)
# 
#      # create data frame listing the number of occurrences of each word
#      #uniqueWords <- data.frame(table(words))
#      unique1grams <- data.frame(table(grams1))
#      unique2grams <- data.frame(table(grams2))
#      unique3grams <- data.frame(table(grams3))
#      unique4grams <- data.frame(table(grams4))
#      
#      gramNames <- c("1-grams (words)", "2-grams", "3-grams", "4-grams")
#      gramVals <-c(num1grams, num2grams, num3grams, num4grams)
#      
#      dfCounts <- data.frame()
#      dfCounts <- data.frame(gramNames, gramVals)
#      colnames(dfCounts) <- c("n.gram.type", "Total.Count")
#      
#      resultList <- list("oneGrams" = grams1, "twoGrams" = grams2,
#                         "threeGrams" = grams3, "fourGrams" = grams4,
#                         "NGramCounts" = dfCounts,
#                         "unique1grams" = unique1grams,
#                         "unique2grams" = unique2grams,
#                         "unique3grams" = unique3grams,
#                         "unique4grams" = unique4grams)
     
     data <- VCorpus(VectorSource(data))
     data <- tm_map(data, stripWhitespace)
     dtm_uni <- DocumentTermMatrix(data)
     dtm_bi <- DocumentTermMatrix(data, control = list(tokenize=BigramTokenizer))
     dtm_tri <- DocumentTermMatrix(data, control = list(tokenize=TrigramTokenizer))
     dtm_quad <- DocumentTermMatrix(data, control = list(tokenize=QuadgramTokenizer))
     numWords <- sum(rowSums(as.matrix(dtm_uni)))
     num2grams <- sum(rowSums(as.matrix(dtm_bi)))
     num3grams <- sum(rowSums(as.matrix(dtm_tri)))
     num4grams <- sum(rowSums(as.matrix(dtm_quad)))

     gramNames <- c("words", "2-grams", "3-grams", "4-grams")
     gramVals <-c(numWords, num2grams, num3grams, num4grams)
     
     dfCounts <- data.frame()
     dfCounts <- data.frame(gramNames, gramVals)
     colnames(dfCounts) <- c("N.Gram.Type", "Total.Count")

     resultList <- list("wordDTM" = dtm_uni,
                        "twoGramDTM" = dtm_bi,
                        "threeGramDTM" = dtm_tri,
                        "fourGramDTM" = dtm_quad,
                        "NGramCounts" = dfCounts)

     return(resultList)
}
# Compute conditional probabilities by Katz's back-off method

convToInt <- function(phrase, dict){
     # function to convert an n-gram phrase into an integer vector
     # dict is the data frame of words and ID
     # phrase is the word vector to be converted
     require(plyr, quietly = TRUE, warn.conflicts = FALSE)
     
     phrase <- tolower(phrase)
     
     # split the prefix into words and count the length
     phrase <- unlist(strsplit(x=phrase, " "))
     n <- length(phrase)
     
     phrase <- mapvalues(phrase, from = dict$words, to=dict$ID, warn_missing=FALSE)
     phrase <- as.integer(phrase)
     phrase[is.na(phrase)] <- 0
     
     return(phrase)     
}

Probability <- function(phrase, probs, tokenized){
     # phrase is an n-gram integer vector
     n <- length(phrase)
     
     if(n==1){
          index <- which(tokenized$uniqueWords$ID == phrase)
          if(length(index) == 0) {
               result <- probs$SGTWords$Probability[1]
          }
          else {
               freq <- tokenized$uniqueWords$Freq[index]
               result <- probs$SGTWords$Probability[which(probs$SGTWords$r == freq)]
          }
     }
     if(n==2){
          index <- which(tokenized$uniqueTwoGrams$V1 == phrase[1] &
                              tokenized$uniqueTwoGrams$V2 == phrase[2])
          if(length(index) == 0) {
               result <- probs$SGTTwoGrams$Probability[1]
          }
          else {
               freq <- tokenized$uniqueTwoGrams$Freq[index]
               result <- probs$SGTTwoGrams$Probability[which(probs$SGTTwoGrams$r == freq)]
          }
     }
     if(n==3){
          index <- which(tokenized$uniqueThreeGrams$V1 == phrase[1] &
                              tokenized$uniqueThreeGrams$V2 == phrase[2] &
                              tokenized$uniqueThreeGrams$V3 == phrase[3])
          if(length(index) == 0) {
               result <- probs$SGTThreeGrams$Probability[1]
          }
          else {
               freq <- tokenized$uniqueThreeGrams$Freq[index]
               result <- probs$SGTThreeGrams$Probability[which(probs$SGTThreeGrams$r == freq)]
          }
     }
     if(n==4){
          index <- which(tokenized$uniqueFourGrams$V1 == phrase[1] &
                              tokenized$uniqueFourGrams$V2 == phrase[2] &
                              tokenized$uniqueFourGrams$V3 == phrase[3] &
                              tokenized$uniqueFourGrams$V4 == phrase[4])
          if(length(index) == 0) {
               result <- probs$SGTFourGrams$Probability[1]
          }
          else {
               freq <- tokenized$uniqueFourGrams$Freq[index]
               result <- probs$SGTFourGrams$Probability[which(probs$SGTFourGrams$r == freq)]
          }
     }  
     return(result)
}

r_rstar <- function(phrase, probs, tokenized){
     # phrase is an n-gram integer vector
     n <- length(phrase)
     
     if(n==1){
          index <- which(tokenized$uniqueWords$ID == phrase)
          if(length(index) == 0) {
               r <- probs$SGTWords$r[1]
               r_star <- probs$SGTWords$r_star[1]
          }
          else {
               r <- tokenized$uniqueWords$Freq[index]
               r_star <- probs$SGTWords$r_star[which(probs$SGTWords$r == r)]
          }
     }
     if(n==2){
          index <- which(tokenized$uniqueTwoGrams$V1 == phrase[1] &
                              tokenized$uniqueTwoGrams$V2 == phrase[2])
          if(length(index) == 0) {
               r <- probs$SGTTwoGrams$r[1]
               r_star <- probs$SGTTwoGrams$r_star[1]
          }
          else {
               r <- tokenized$uniqueTwoGrams$Freq[index]
               r_star <- probs$SGTTwoGrams$r_star[which(probs$SGTTwoGrams$r == r)]
          }
     }
     if(n==3){
          index <- which(tokenized$uniqueThreeGrams$V1 == phrase[1] &
                              tokenized$uniqueThreeGrams$V2 == phrase[2] &
                              tokenized$uniqueThreeGrams$V3 == phrase[3])
          if(length(index) == 0) {
               r <- probs$SGTThreeGrams$r[1]
               r_star <- probs$SGTThreeGrams$r_star[1]
          }
          else {
               r <- tokenized$uniqueThreeGrams$Freq[index]
               r_star <- probs$SGTThreeGrams$r_star[which(probs$SGTThreeGrams$r == r)]
          }
     }
     if(n==4){
          index <- which(tokenized$uniqueFourGrams$V1 == phrase[1] &
                              tokenized$uniqueFourGrams$V2 == phrase[2] &
                              tokenized$uniqueFourGrams$V3 == phrase[3] &
                              tokenized$uniqueFourGrams$V4 == phrase[4])
          if(length(index) == 0) {
               r <- probs$SGTFourGrams$r[1]
               r_star <- probs$SGTFourGrams$r_star[1]
          }
          else {
               r <- tokenized$uniqueFourGrams$Freq[index]
               r_star <- probs$SGTFourGrams$r_star[which(probs$SGTFourGrams$r == r)]
          }
     }
     result <- as.integer(c(r, r_star))
     return(result)
}

condProb <- function(word, prefix, probs, tokenized){
     # probs is the list of n-gram probabilities
     # word is the integer form of the word
     # prefix is the conditioning n-gram integer vector
     
     numerator <- c(prefix, word)
     numerator <- convToInt(numerator, tokenized$uniqueWords)
     prefix <- convToInt(prefix, tokenized$uniqueWords)
     
     # return standard probability if prefix is empty
     if(length(prefix)==0){
          word <- convToInt(word, tokenized$uniqueWords)
          wordProb <- Probability(word, probs, tokenized)
          return(wordProb)
     }
     
     numerator_r_rstar <- r_rstar(numerator, probs, tokenized)
     prefix_r_rstar <- r_rstar(prefix, probs, tokenized)
     
     if(numerator_r_rstar[1]>0){
          result <- numerator_r_rstar[2]/prefix_r_rstar[1]
          return(result)
     }
     else {
          len <- length(prefix)-1
          prefix <- prefix[1:len]
          return(0.4*condProb(word, prefix, probs, tokenized))
     }   
}
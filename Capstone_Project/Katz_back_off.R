# Compute conditional probabilities by Katz's back-off method

condProb <- function(word, prefix, dict, probs){
     # dict is the data frame of words and ID
     # word is the word to be predicted
     # prefix is the conditioning n-gram
     
     require(plyr, quietly = TRUE, warn.conflicts = FALSE)
     
     # make sure that word and prefix are lowercase
     word <- tolower(word)
     prefix <- tolower(prefix)
     
     # split the prefix into words and count the length
     prefix <- unlist(strsplit(x=prefix, " "))
     n <- length(prefix)
     
#      print(word)
#      print(prefix)
     
     # Convert the word and prefix into integers
     word <- mapvalues(word, from = dict$words, to=dict$ID, warn_missing=FALSE)
     prefix <- mapvalues(prefix, from = dict$words, to=dict$ID, warn_missing=FALSE)
     word <- as.integer(word)
     prefix <- as.integer(prefix)
     
#      print(word)
#      print(prefix)

     
     
     
     return(prefix)     
}
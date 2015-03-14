# Function to tokenize and filter profanity form the text

Tokenize <- function(fileStr, n =-1L) {
     #require(tm, quietly = TRUE)
     
     fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
     fileStr <- paste0(fileDir, fileStr)
     
     # create a connection for the file
     con <- file(fileStr, open="r")
     
     # read in the lines and close the connection
     lines <- readLines(con, n=n) 
     close(con)
     
     # split the lines by punctuation (except apostrophe) and unlist
     words = unlist(strsplit(lines, "(?!')[[:punct:]]|[[:space:]]", perl = T))   

     # remove all ""
     words <- words[words != ""]
     
     # convert all letters to lowercase
     words <- tolower(words)
     
     # replace all single digit numbers by the word form
     require(qdap, quietly = TRUE)
     nums = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
     numAsWord = c("one", "two", "three", "four", "five", "six", "seven",
                   "eight", "nine", "zero")
     
     words <- mgsub(nums, numAsWord, words)
     
     # Number of tokens
     tokens = length(words)
     
     # Unique word list (sorted)
     uniqueWords <- sort(unique(words))
     
     # Number of types
     types = length(uniqueWords)
     
     # create data frame listing the number of occurrences of each word
     uniqueWords <- data.frame(table(words))

     
     resultList <- list("words" = words, "numTokens" = tokens, 
                        "numTypes" = types, "uniqueWords" = uniqueWords)
     
     return(resultList)
}

linesBlogs <- Tokenize("en_US.blogs.txt", 5)
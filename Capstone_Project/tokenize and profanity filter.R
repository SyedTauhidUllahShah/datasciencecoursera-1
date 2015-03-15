# Function to tokenize and filter profanity form the text

Tokenize <- function(fileStr, n =-1L) {
     require(qdap, quietly = TRUE)
     #require(tm, quietly = TRUE)
     
     fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
     fileStr <- paste0(fileDir, fileStr)
     
     # create a connection for the file
     con <- file(fileStr, open="r")
     
     # read in the lines and close the connection
     lines <- readLines(con, n=n) 
     close(con)
     
     # conert all text to lower case
     lines <- tolower(lines)
     
     # formal prefix abbreviations
     prefixesFound <- c("mr.", "sr.", "jr.", "fr.")
     prefixesRepl <- c("mister", "senior", "junior", "father")
     lines <- mgsub(prefixesFound, prefixesRepl, lines)
     
     # Replace contractions with full words using the contractions dataset
     lines <- mgsub(contractions$contraction, contractions$expanded, lines)
     
     # numerals with words
     numOrderFound <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th",
                        "8th", "9th", "0th", "10th")
     numOrderRepl <- c("first", "second", "third", "fourth", "fifth",
                       "sixth", "seventh", "eighth", "nineth", "zeroth",
                       "tenth")
     lines <- mgsub(numOrderFound, numOrderRepl, lines)
     
     # split the lines by punctuation (except apostrophe) and unlist
     words = unlist(strsplit(lines, "(?!')[[:punct:]]|[[:space:]]", perl = T))   

     # remove all ""
     words <- words[words != ""]
     
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

tokenizedBlogs <- Tokenize("en_US.blogs.txt", 10)
tokenizedNews <- Tokenize("en_US.news.txt", 10)
tokenizedTwitter <- Tokenize("en_US.twitter.txt", 10)
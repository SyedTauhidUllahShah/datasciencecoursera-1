# Function to tokenize and filter profanity form the text

Tokenize <- function(fileStr, n =-1L) {
     require(qdap, quietly = TRUE, warn.conflicts = FALSE)
     require(SnowballC, warn.conflicts = FALSE, quietly = TRUE)
     require(tm, quietly = TRUE, warn.conflicts = FALSE)
     
     fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/sampled/"
     fileStr <- paste0(fileDir, fileStr)
     
     # create a connection for the file
     con <- file(fileStr, open="r")
     
     # read in the lines and close the connection
     lines <- readLines(con, n=n) 
     close(con)
     
     # conert all text to lower case
     lines <- tolower(lines)
     
     # Profanity filtering - set all profanity to EXPLICATIVE
     profanityFound = c("shit", "asshole", "fuck", "fucking", "cunt", "damn",
                        "bitch", "cock sucker", "dickhead", "dick head",
                        "mother fucker", "fucker")
     lines <- mgsub(profanityFound, "EXPLICATIVE", lines)
     
     # replace unicode symbols
     unicodeFound <- c("\u2019","\u201D","\u201C","\U0001f466",
                       "\u0093i\u0092m", "\u0094","\u0093i")
     unicodeRepl <- c("'","","","","","","")
     lines <- mgsub(unicodeFound, unicodeRepl, lines)
     
     # formal prefix abbreviations
     prefixesFound <- c("mr.", "sr.", "jr.", "fr.", "btw", "fyi")
     prefixesRepl <- c("mister", "senior", "junior", "father",
                       "by the way", "for your information")
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
     #words = unlist(strsplit(lines, "(?!')[[:punct:]]|[[:space:]]", perl = T))
     #words = MC_tokenizer(lines)
     
     # determine where the ngrams (up to n=4) lie
     z <- ngrams(lines, n=4)
     grams <- unlist(z$raw)
     gramsList = sapply(gregexpr("\\w+", grams), length)
     
     grams1 <- grams[gramsList == 1]
     grams2 <- grams[gramsList == 2]
     grams3 <- grams[gramsList == 3]
     grams4 <- grams[gramsList == 4]

     # Number of tokens
     #tokens = length(words)
     num1grams = length(grams1)
     num2grams = length(grams2)
     num3grams = length(grams3)
     num4grams = length(grams4)

     # create data frame listing the number of occurrences of each word
     #uniqueWords <- data.frame(table(words))
     unique1grams <- data.frame(table(grams1))
     unique2grams <- data.frame(table(grams2))
     unique3grams <- data.frame(table(grams3))
     unique4grams <- data.frame(table(grams4))

     resultList <- list("oneGrams" = grams1, "num1grams" = num1grams,
                        "twoGrams" = grams2, "num2grams" = num2grams,
                        "threeGrams" = grams3, "num3grams" = num3grams,
                        "fourGrams" = grams4, "num4grams" = num4grams,
                        "unique1grams" = unique1grams,
                        "unique2grams" = unique2grams,
                        "unique3grams" = unique3grams,
                        "unique4grams" = unique4grams)
     
     return(resultList)
}
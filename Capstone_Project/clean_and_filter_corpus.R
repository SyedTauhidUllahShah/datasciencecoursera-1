# Function to tokenize and filter profanity form the text

readLinesFile <- function(filePath, n=-1L){
     # create a connection for the file
     con <- file(filePath, open="r")
     
     # read in the lines and close the connection
     lines <- readLines(con, n=n) 
     close(con)
     
     return(lines)
}

clean_and_filter_corpus <- function(lines) {
     require(qdap, quietly = TRUE, warn.conflicts = FALSE)
     require(stringr, quietly = TRUE, warn.conflicts = FALSE)
     require(SnowballC, warn.conflicts = FALSE, quietly = TRUE)
     require(tm, quietly = TRUE, warn.conflicts = FALSE)
     require(RWeka, quietly = TRUE, warn.conflicts = FALSE)

     # read in bad words file
     badwords <- readLinesFile('~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/badwords.txt')
     
     # convert all letters to lowercase, remove punctuation, numbers, whitespace
     doc.vec <- VectorSource(lines)
     doc.corpus <- Corpus(doc.vec)
     doc.corpus.copy <- doc.corpus
     doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
     doc.corpus <- tm_map(doc.corpus, removePunctuation)
     doc.corpus <- tm_map(doc.corpus, removeNumbers)
     #doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
     doc.corpus <- tm_map(doc.corpus, stripWhitespace)
     
     # Remove profanity
     profanewordsvector <- VectorSource(badwords)
     doc.corpus <- tm_map(doc.corpus, removeWords, profanewordsvector)

     # Stem Document
     #doc.corpus <- tm_map(doc.corpus, stemDocument)
     
     # Stem complete
     #doc.corpus <- tm_map(doc.corpus, stemCompletion, dictionary = doc.corpus.copy)
     
     # convert to character vector
     doc.corpus <- unlist(sapply(doc.corpus, '[', "content"))
     
     return(doc.corpus)
}
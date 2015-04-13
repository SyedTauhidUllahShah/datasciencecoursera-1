# Regression model pre-processing

# 4GB RAM for Java
options(java.parameters = "-Xmx4g")

source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/clean_and_filter_corpus.R')
source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/Katz_back_off.R')
source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/tokenize_file.R')
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

# Create Document Term Matrices
corpus <- do.call(function(...) c(..., recursive = FALSE), cleanedData)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.98)

# Get n-Grams
BiGrams <- lapply(cleanedData, BiGramTokenizer)
TriGrams <- lapply(cleanedData, TriGramTokenizer)
QuadGrams <- lapply(cleanedData, QuadGramTokenizer)

# Create document term matrices for the n-grams
rm(cleanedData)
BiGrams <- lapply(BiGrams, VectorSource)
corpus <- do.call(function(...) c(..., recursive = FALSE), BiGrams)
corpus <- unlist(corpus)
bi.dtm <- DocumentTermMatrix(corpus)
bi.dtm <- removeSparseTerms(bi.dtm, 0.98)
rm(BiGrams)
corpus <- do.call(function(...) c(..., recursive = FALSE), TriGrams)
tri.dtm <- DocumentTermMatrix(corpus)
tri.dtm <- removeSparseTerms(tri.dtm, 0.98)
rm(TriGrams)
corpus <- do.call(function(...) c(..., recursive = FALSE), QuadGrams)
quad.dtm <- DocumentTermMatrix(corpus)
quad.dtm <- removeSparseTerms(quad.dtm, 0.98)
rm(QuadGrams)

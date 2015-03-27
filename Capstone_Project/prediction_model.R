# Code to generate language prediction model

# 1. clean and filter the data
source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/clean_and_filter.R')
fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
US.path <- paste0(fileDir,"sampled/en_US.txt")
#cleanedCombined <- clean_and_filter(US.path)

# 2. Tokenize to get words and n-grams
source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/tokenize_file.R')
#tokenCombined <- Tokenize(cleanedCombined)

# 3. Run simple Good-Turing to calculate the probabilities
tokenCombinedProbs <- GTProbs(tokenCombined)

# 4. Use Katz back-off to compute conditional probabilities

# 5. Compute the conditional expectation to make prediction

# Function to product list of Good-Turing probabilities for the n-grams

GTProbs <- function(lst){
     # lst is the tokenized list
     
     source('~/Documents/School/Johns Hopkins Data Science/datasciencecoursera/Capstone_Project/Simple_Good_Turing.R')
     
     ProbWords <- SimpleGoodTuring(lst$FreqOfFreqWords$Freq,
                                   lst$FreqOfFreqWords$Nr)
     ProbTwoGrams <- SimpleGoodTuring(lst$FreqOfFreqTwoGrams$Freq,
                                   lst$FreqOfFreqTwoGrams$Nr)
     ProbThreeGrams <- SimpleGoodTuring(lst$FreqOfFreqThreeGrams$Freq,
                                      lst$FreqOfFreqThreeGrams$Nr)
     ProbFourGrams <- SimpleGoodTuring(lst$FreqOfFreqFourGrams$Freq,
                                        lst$FreqOfFreqFourGrams$Nr)
     result <- list("SGTWords" = ProbWords,
                    "SGTTwoGrams" = ProbTwoGrams,
                    "SGTThreeGrams" = ProbThreeGrams,
                    "SGTFourGrams" = ProbFourGrams)
     
     return(result)
}
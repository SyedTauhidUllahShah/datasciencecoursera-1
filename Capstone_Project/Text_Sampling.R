# text sampling for the english corpora
# uses binomial sampling to select the lines to read in

numLinesTwitter <- 2360148
numLinesBlogs <- 899288
numLinesNews <- 1010242

fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
TwitterStr <- paste0(fileDir, "en_US.twitter.txt")
newsStr <- paste0(fileDir, "en_US.news.txt")
blogsStr <- paste0(fileDir, "en_US.blogs.txt")

textSample <- function(fileStr, nLines, p = .25){
     
     filePath <- paste0(fileDir, fileStr)
     
     # create a connection for the file
     con <- file(filePath, open="r")

     # read in the lines and close the connection
     lines <- readLines(con) 
     close(con)

     # Generate random sample
     set.seed(12321)
     samples <- rbinom(n = nLines, size = 1, prob = p)
     lines <- lines[which(samples==1)]

     # write the lines to a file
     filePath <- paste0(fileDir,"sampled/",fileStr)
     con <- file(filePath, open="w")

     writeLines(lines, con)
     close(con)
}

# sampling probability
p = .01

setwd(paste0(fileDir,"/sampled/"))
system("rm en*.txt")

textSample("en_US.twitter.txt", numLinesTwitter, p)
textSample("en_US.news.txt", numLinesNews, p)
textSample("en_US.blogs.txt", numLinesBlogs, p)

system("cat *.txt > en_US.txt")

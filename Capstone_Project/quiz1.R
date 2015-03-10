# Read in files
fileDir <- "~/Documents/School/Coursera Data Science/Capstone Project/final/en_US/"
fileBlogs <- paste0(fileDir,"en_US.blogs.txt")
fileNews <- paste0(fileDir,"en_US.news.txt")
fileTwitter <- paste0(fileDir,"en_US.twitter.txt")

conBlogs <- file(fileBlogs, open="r")
conNews <- file(fileNews, open="r")
conTwitter <- file(fileTwitter, open="r")

txtBlogs <- readLines(conBlogs)
close(conBlogs)
txtNews <- readLines(conNews)
close(conNews)
txtTwitter <- readLines(conTwitter)
close(conTwitter)
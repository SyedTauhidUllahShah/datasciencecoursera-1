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

# Longest line in each file
maxLgBlogs <- max(nchar(txtBlogs[1:length(txtBlogs)]))
maxLgNews <- max(nchar(txtNews[1:length(txtNews)]))
maxLgTwitter <- max(nchar(txtTwitter[1:length(txtTwitter)]))

# find "love" and "hate" in the en_Us.twitter data set
loveTwitter = length(grep("love",txtTwitter))
hateTwitter = length(grep("hate",txtTwitter))
ratio = loveTwitter/hateTwitter

# line in twitter that had "biostats" says
biostatsTwitterLine = txtTwitter[grep("biostats",txtTwitter)]

# long line grep
Tweet1 <- length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", txtTwitter))

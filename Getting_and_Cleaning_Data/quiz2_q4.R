require("RCurl")
#library(XML)

con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
htmlCode
print(nchar(htmlCode[10]))
print(nchar(htmlCode[20]))
print(nchar(htmlCode[30]))
print(nchar(htmlCode[100]))
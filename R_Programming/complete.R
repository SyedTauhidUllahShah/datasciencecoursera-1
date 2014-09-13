complete <- function(directory, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the
     ## number of complete cases
     
     # Convert id number to string with three digits and generate filename
     file_name <- vector()
     for (i in seq_along(id)) {
          three_digit_prefix <- sprintf("%.03d",id[i])
          file_name[i]<-paste(directory,"/",three_digit_prefix,".csv",sep="")
     }
     
     # For each file, read in csv and compute number of complete observations
     num_files<-length(file_name)
     nobs <- vector()
     for (i in 1:num_files) {
               dat <- read.csv(file_name[i])
               nobs[i] <- sum(complete.cases(dat))
     }
     df = data.frame(id,nobs)
     return(df)
}
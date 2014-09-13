corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations
     
     # Use complete.R to retrieve a data frame that counts the number
     # of complete cases in each file in the directory
     
     dat <- complete(directory)
     
     # Subset data frame for rows whose value > threshold
     dat <-subset(dat,nobs>=threshold)
     
     # retrieve id list from subsetted data frame
     id <- dat[,"id"]
     
     # If Length of id list == 0, return vector of length 0
     if(length(id)==0) {
          return(numeric())
     }
     
     # Convert id number to string with three digits and generate filename
     file_name <- vector()
     for (i in seq_along(id)) {
          three_digit_prefix <- sprintf("%.03d",id[i])
          file_name[i]<-paste(directory,"/",three_digit_prefix,".csv",sep="")
     }
     
     # For each file, read in csv and compute correlation of sulfate and nitrate
     num_files<-length(file_name)
     corr_result <-numeric()
     for (i in 1:num_files){
          datCorr <- read.csv(file_name[i])
          complete_cases_yn = complete.cases(datCorr) 
          datCorr <-datCorr[complete_cases_yn,]
          corr_result[i] <- cor(datCorr[,"sulfate"],datCorr[,"nitrate"])         
     }
     
     return(corr_result)
     
     # 
     
}
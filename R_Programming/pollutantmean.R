pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
     
     # Convert id number to string with three digits and generate filename
     file_name <- vector()
     for (i in seq_along(id)) {
          three_digit_prefix <- sprintf("%.03d",id[i])
          file_name[i]<-paste(directory,"/",three_digit_prefix,".csv",sep="")
     }
     
     # For each file, read in csv and append to data frame
     num_files<-length(file_name)
     dat <- read.csv(file_name[1])
     if(num_files>=2){
          for (i in 2:num_files) {
               dat_append <- read.csv(file_name[i])
               dat <-rbind(dat,dat_append)
          }
     }

     # Subset the data using the pollutant and remove the NAs
     dat <- dat[,pollutant]
     dat <- dat[!is.na(dat)]
     
     # compute and return the mean
     dat_mean = round(mean(dat),digits=3)
     dat_mean
}
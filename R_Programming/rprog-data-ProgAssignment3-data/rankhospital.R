rankhospital <- function(state, outcome, num="best"){
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     set_of_outcomes <-c("heart attack","heart failure","pneumonia")
     if((state %in% data$State)==FALSE) stop("invalid state")
     if((outcome %in% set_of_outcomes)==FALSE) stop("invalid outcome")
     
     ## Rename the outcome column of interest in the data set   
     if(outcome==set_of_outcomes[1]) col_Index <- 11
     if(outcome==set_of_outcomes[2]) col_Index <- 17
     if(outcome==set_of_outcomes[3]) col_Index <- 23
     names(data)[col_Index] <-"Outcome"   
     
     ## Convert the outcome column data to numeric
     data[,col_Index] <- as.numeric(data[,col_Index])
     
     ## Subset the data to only include hospitals from state
     data <- data[data$State==state,]
     
     ## Remove all NAs from the outcome column
     data <- data[complete.cases(data[,col_Index]),]
     
     ## Return NA if num ranking is greater than the number of rows in the
     ## reduced data set
     
     if(is.numeric(num)==TRUE){
          if(nrow(data)<as.numeric(num)) {
               return(NA)
          } else {
               data <- data[order(data$Outcome),]
               data <-data[num,]
          }         
     }
       
     ## If num=="best", sort in accending order
     if(num=="best") {
          data <- data[order(data$Outcome),]
          data <- data[1,]
     }
     ## If num=="worst", sort in descending order
     if(num=="worst") {
          data <- data[order(-data$Outcome),]
          data <- data[1,]
     }
     
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
     result <- data$Hospital.Name
     result
}
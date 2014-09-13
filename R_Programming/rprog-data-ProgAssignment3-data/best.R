best <- function(state, outcome) {
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
     
     ## Compute the minimum 30-day mortality rate
     minMortality <- min(data[,col_Index],na.rm=TRUE)
     
     ## Subset the data to include those hospitals that have the minimum
     ## mortality rate and no NA
     BestHosp <- data[data$Outcome==minMortality,]
     BestHosp <- BestHosp[complete.cases(BestHosp$Outcome),]
     BestHospSort <- BestHosp[order(BestHosp$Hospital.Name),]
     BH <- BestHospSort$Hospital.Name
     
     ## Return hospital name in that state with lowest 30-day death rate
     BH
}
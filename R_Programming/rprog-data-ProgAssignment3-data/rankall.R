rankall <- function(outcome, num="best"){
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that outcome is valid
     set_of_outcomes <-c("heart attack","heart failure","pneumonia")
     if((outcome %in% set_of_outcomes)==FALSE) stop("invalid outcome")
     
     ## Rename the outcome column of interest in the data set   
     if(outcome==set_of_outcomes[1]) col_Index <- 11
     if(outcome==set_of_outcomes[2]) col_Index <- 17
     if(outcome==set_of_outcomes[3]) col_Index <- 23
     names(data)[col_Index] <-"Outcome"   
     
     ## Convert the outcome column data to numeric
     data[,col_Index] <- as.numeric(data[,col_Index])
     
     ## Determine the list of states
     states <- unique(data$State)
     states <- states[order(states)]
     numStates <- length(states)
     
     ## Generate data frame with Hospital.Name, state, and outcome as columns
     data <- data.frame(data$Hospital.Name, data$State, data$Outcome)
     colnames(data)<-c("hospital","state","Outcome")
     
     ## Remove all NAs from the outcome column
     data <- data[complete.cases(data$Outcome),]
     
     ## Loop through each state ranking the hospitals according to num
     result <-data.frame(hospital=character(),state=character())
     for(i in 1:numStates) {
          dataPerState <- data[data$state==states[i],]
          colnames(dataPerState)<-c("hospital","state","Outcome")
          if(is.numeric(num)==TRUE) {
               if(nrow(dataPerState)<as.numeric(num)) {
                    dataTemp <- data.frame(NA,states[i])
               } else {
                    dataPerState <- with(dataPerState,
                                         dataPerState[order(dataPerState$Outcome,
                                                       dataPerState$hospital),])
                    dataPerState <-dataPerState[num,]
                    dataTemp <- data.frame(dataPerState$hospital,
                                           dataPerState$state)
               }         
          }
         
          ## If num=="best", sort in accending order
          if(num=="best") {
               dataPerState <- with(dataPerState,
                                    dataPerState[order(dataPerState$Outcome,
                                                   dataPerState$hospital),])
               dataPerState <- dataPerState[1,]
               dataTemp <- data.frame(dataPerState$hospital,
                                      dataPerState$state)
          }
          ## If num=="worst", sort in descending order
          if(num=="worst") {
               dataPerState <- with(dataPerState,
                                    dataPerState[order(-dataPerState$Outcome,
                                                       dataPerState$hospital),])
               dataPerState <- dataPerState[1,]
               dataTemp <- data.frame(dataPerState$hospital,
                                      dataPerState$state)
          }
          colnames(dataTemp) <- c("hospital","state")      
          data_append <- data.frame(dataTemp$hospital, dataTemp$state)
          result <-rbind(result,data_append)
     }
     colnames(result) <- c("hospital","state")
     
#      
#      ## Return NA if num ranking is greater than the number of rows in the
#      ## reduced data set
#        
#      ## If num=="best", sort in accending order
#      if(num=="best") {
#           data <- data[order(data$Outcome),]
#           data <- data[1,]
#      }
#      ## If num=="worst", sort in descending order
#      if(num=="worst") {
#           data <- data[order(-data$Outcome),]
#           data <- data[1,]
#      }
#      
      ## Return hospital name in each state with the given rank
      ## 30-day death rate

     result
}
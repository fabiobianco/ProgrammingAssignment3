## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name 
## of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state
##
best2 <- function(state, outcome){
        ## read the data from files
        data <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
        hospital_data <- read.csv("./hospital-data.csv", colClasses = "character")
        ## create a new dataset with four colomn: 1 - State, 2 - Heart Attack, 3 - Heart Faiule, 4 - Pneumonia
        ## 
        data_outcome <- data.frame(hospital_name=data[,2], hospital_state=data[, 7], heart_attack=data[, 11], heart_failure=data[, 17], pneumonia=data[, 23])
        ##  
        data_outcome[, 1] <- as.character(data_outcome[ ,1])
        data_outcome[, 2] <- as.character(data_outcome[ ,2])                                  
        data_outcome[, 3] <- as.numeric(levels(data_outcome[ ,3]))[data_outcome[, 3]]
        data_outcome[, 4] <- as.numeric(levels(data_outcome[ ,4]))[data_outcome[, 4]]
        data_outcome[, 5] <- as.numeric(levels(data_outcome[ ,5]))[data_outcome[, 5]]
        ##
        hospital_data[, 7] <- as.character(hospital_data[ ,7])
        ## Check if state argument is correct
        if(!(state %in% hospital_data[, 7]))
        {
                stop("invalid state")  
        }
        data_outcome <- data_outcome[complete.cases(data_outcome), ]
        ##print(str(data_outcome))
        ##
        ## Order the dataset by outcome and state
        ##
        data_outcome <- subset(data_outcome, hospital_state == state)
        ##print(head(data_outcome))
        ##
        ## create a subset for the specific outcome
        ##
        i <- 0
        if (outcome == "heart attack")
        {
                i <- i + 1
                data_outcome <- data_outcome[ order(data_outcome[,3], data_outcome[,1]), ]        
        }
        ## Order the dataset by outcome and state
        if (outcome == "heart failure")
        {
                i <- i + 1
                data_outcome <- data_outcome[ order(data_outcome[,4], data_outcome[,1]), ]
        }
        if (outcome == "pneumonia")
        {
                i <- i + 1
                data_outcome <- data_outcome[ order(data_outcome[,5], data_outcome[,1]), ]
        }
        if (i == 0) { stop("invalid outcome")}
        ## Print the Hospital Name!
        return(data_outcome)
}
##
best3 <- function(outcome){
        ## read the data from files
        data <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
        ## create a new dataset with four colomn: 1 - State, 2 - Heart Attack, 3 - Heart Faiule, 4 - Pneumonia
        ## 
        data_outcome <- data.frame(hospital=data[,2], state=data[, 7], heart_attack=data[, 11], heart_failure=data[, 17], pneumonia=data[, 23])
        ##  
        data_outcome[, 1] <- as.character(data_outcome[ ,1])
        data_outcome[, 2] <- as.character(data_outcome[ ,2])                                  
        data_outcome[, 3] <- as.numeric(levels(data_outcome[ ,3]))[data_outcome[, 3]]
        data_outcome[, 4] <- as.numeric(levels(data_outcome[ ,4]))[data_outcome[, 4]]
        data_outcome[, 5] <- as.numeric(levels(data_outcome[ ,5]))[data_outcome[, 5]]

        ##data_outcome <- data_outcome[complete.cases(data_outcome), ]
        ## create a dataset list by state
        data_outcome <- split(data_outcome, data_outcome$state)
        ##print(str(data_outcome))
        ## Insert the new rank column in the datasets
        i <- 0
        if (outcome == "heart attack")
        {
                i <- i + 1
                data_outcome <- lapply(data_outcome, function(x) x[order(x$heart_attack, x$hospital), ]) 
        }
        if (outcome == "heart failure")
        {
                i <- i + 1
                data_outcome <- lapply(data_outcome, function(x) x[order(x$heart_failure, x$hospital), ])
        }
        if (outcome == "pneumonia")
        {
                i <- i + 1
                data_outcome <- lapply(data_outcome, function(x) x[order(x$pneumonia, x$hospital), ])
        }
        if (i == 0) { stop("invalid outcome")}
        ##print(data_outcome[[51]])
        return(data_outcome)
        ##return(data_outcome[[3]])
        ##return(data_outcome[[50]])
}
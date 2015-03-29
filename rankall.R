rankall <- function(outcome, num = "best"){
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospital_data <- read.csv("./hospital-data.csv", colClasses = "character")
        hospital_data[, 7] <- as.character(hospital_data[ ,7])
        ##
        staterank <- best3(outcome)
        ##
        hospital_state <- data.frame()
        if(num == "best")
        { num <- 1}
        
        for (i in 1:54) {
                hospital_state_new <- data.frame(hospital=staterank[[i]][num, 1], state=staterank[[i]][num, 2])
                hospital_state <- rbind(hospital_state,hospital_state_new)
                rm(hospital_state_new)
        }
        hospital_state <- hospital_state[complete.cases(hospital_state), ]
        if(num == "worst")
        { 
                for (i in 1:54){
                        hospital_state_new <- data.frame(hospital=staterank[[i]][nrow(staterank[[i]]), 1], state=staterank[[i]][nrow(staterank[[i]]), 2])
                        hospital_state <- rbind(hospital_state,hospital_state_new)
                        rm(hospital_state_new)
                        print(nrow(staterank[[i]]))
                }
                print(head(hospital_state))
        }
        ##hospital_state <- hospital_state[complete.cases(hospital_state), ]
        hospital_state 
}
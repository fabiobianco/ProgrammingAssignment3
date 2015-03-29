## Return hospital name in that state with the given rank
## 30-day death rate
rankhospital <- function(state, outcome, num = "best")
{
        ## Read the dataset via best2 function
        datarank <- best2(state, outcome)
        ## Create the rank vector
        rank = c(1:nrow(datarank))
        print(dim(datarank))
        ## Insert the new colomn in the dataset
        datarank <- cbind(datarank, rank)
        ## Check that state and outcome are valid
        ## if num argument ugual "best" case
        if ( num == "best"){
                return(datarank[1,1])    
        }
        ## if num argument ugual "worst" case
        if ( num == "worst"){
                return(datarank[nrow(datarank),1])
        }
        ## if num is a number 
        num <- as.numeric(num)
        print(num)
        if (num > nrow(datarank)){ return("NA")}
        ## create a vector for the specific num rank
        hospital_rank <- subset(datarank, rank == num)
        ## Read outcome data
        return(hospital_rank[1, 1])
}
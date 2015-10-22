best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        st.name <- unique(data[,7])
        if( !any(st.name == state)) stop("invalid state")
        if(outcome == "heart attack"){
                data[,11] <- as.numeric(data[,11])
                my_data <- data[,c(2,7,11)]
        }else if(outcome == "heart failure"){
                data[,17] <- as.numeric(data[,17])
                my_data <- data[,c(2,7,17)]
        }else if(outcome == "pneumonia"){
                data[,23] <- as.numeric(data[,23])
                my_data <- data[,c(2,7,23)]
        }
        else stop("invalid outcome")
        my_data <- my_data[complete.cases(my_data),]
        hos_data <- my_data[my_data$State == state,]
        new_data <- tapply(hos_data[,1],hos_data[,3],sort)
        best <- as.character(new_data[1])
        best
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}

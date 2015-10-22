rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
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
        df <- split(my_data,my_data$State)
        new_data <- data.frame(hospital=c(NA),state=c(NA),stringsAsFactors = FALSE)
        ## For each state, find the hospital of the given rank
        for(i in 1:54){
                 st_data <- df[[i]]
                 rank_data <- st_data[order(st_data[3],st_data[1]),]
                 return_rank <- if(num == "best"){
                         1
                 }else if(num == "worst"){
                         nrow(rank_data)
                 }else{
                         as.numeric(num)
                 }
                 new_data[i,1] <- rank_data[return_rank,1]
                 new_data[i,2] <- unique(st_data[2])
        }
        new_data
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}

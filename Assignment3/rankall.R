rankall <- function(outcome, num = "best"){
    
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    if (!(outcome %in% names(outcomes))){
        stop("invalid outcome")
    }
    
    #read in the data, subset it, rename columns
    hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    hdata <- data.frame(c(hdata[2], hdata[7], hdata[outcomes[outcome]]))
    names(hdata) <- c("Hospital", "State", "Outcome")
    
    #sort the data 
    hdata <- hdata[order(hdata$State, hdata$Outcome, hdata$Hospital), ]
    
    #remove NAs
    hdata <- na.omit(hdata)
    
    #split the data by state
    #hdataList <- split(hdata, hdata$State)
    #hdataList <- split(hdata[,c("Hospital")], hdata$State)  #this one worked, nay need to revert
    hdataList <- split(hdata$Hospital, hdata$State)
    
    #define function to use in lapply
    rank <-function(x, num){
        if (num=="best"){head(x, 1)}
        else if (num=="worst"){tail(x,1)}
        else {x[num]}
    }
    
    #result <- lapply(hdataList, rank, num)
    #unlist(result)
    result <- sapply(hdataList, rank, num, simplify= TRUE)
    list_names <- names(result) 
    return (data.frame(hospital = unlist(result), state = names(result), row.names=names(result)))
    
}
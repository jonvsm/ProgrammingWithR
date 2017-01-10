rankhospital <- function(state, outcome, num="best"){

    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    if (!(outcome %in% names(outcomes))){
        stop("invalid outcome")
    }
    
    #read in and subset, rename data
    hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    hdata <- data.frame(c(hdata[2], hdata[7], hdata[outcomes[outcome]]))
    names(hdata) <- c("Hospital", "State", "Outcome")
    hdata <- na.omit(hdata)
    
    #test state input
    if (!(state %in% hdata$State)) {
        stop("invalid state")
    }
   
    #sort data 
    hdata <- hdata[order(hdata$State, hdata$Outcome, hdata$Hospital), ]
    
    #subset state
    hdata <- hdata[hdata$State==state, ]
    if (num=="best"){result=hdata[1,1]}
    else if (num=="worst"){result=hdata[dim(hdata)[1],1]}
    else{result=hdata[num,1]}
    
    return (as.character(result))
}

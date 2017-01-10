best <- function(state, outcome){
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
    hdataSt <- hdata[hdata$State==state, ]
    
    result=hdataSt[1,1]
    print(as.character(result))

}
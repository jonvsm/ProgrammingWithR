complete <- function(directory, id=1:332){
    rows <- length(id)
    dframe <- data.frame(matrix(nrow=rows, ncol = 2))
    colnames(dframe) <- c("id","nobs")
    r=1
    c=1
    
    for (eachmonitor in id){
        #print(r)
        padmon <- str_pad(eachmonitor, 3, pad = "0")
        file_name = paste(directory, padmon, ".csv", sep="")
        polfile = read.csv(file_name)
        cc <- sum(complete.cases(polfile)=="TRUE")
        dframe[r,c] <- padmon
        dframe[r,(c+1)] <- cc 
        r=r+1
        c=1
    }
    dframe
}
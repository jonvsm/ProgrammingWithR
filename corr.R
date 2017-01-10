corr <- function(directory, threshold=0){
    id <- 1:332
    result <- numeric()
    
    #read files and find number of complete cases
    for (eachmonitor in id){
        padmon <- str_pad(eachmonitor, 3, pad = "0")            #add leading zeros to id where needed.  This call requires library(stringr)
        file_name = paste(directory, padmon, ".csv", sep="")    #build up the file name
        #print(file_name)
        polfile = read.csv(file_name)                           #read the file
        cc <- sum(complete.cases(polfile)=="TRUE")              #calculate number of complete cases
        #print(cc)
        if (cc > threshold){                                    #if complete cases is greater than threshold, calc cor
            #print("yes")
            #polfileCompleteCases <- polfile[complete.cases(polfile), ] #filter to only complete cases
            #correlation <- cor(polfileCompleteCases$nitrate, polfileCompleteCases$sulfate)
            correlation <- cor(polfile$nitrate, polfile$sulfate, use="complete.obs")
            
            #print(correlation)
            result <- c(result, correlation)
        }
      }
    result
}
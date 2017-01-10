pollutantmean <- function(directory="C:/R/rprog_data_specdata/specdata/", pollutant, id = 1:332) {
    library(stringr)
    
    if (pollutant=="nitrate"){i=3}
    else {i = 2}
    
    polvector <- numeric()
    
    for (eachmonitor in id){
        padmon <- str_pad(eachmonitor, 3, pad = "0")
        file_name = paste(directory, padmon, ".csv", sep="")
        #print(file_name)
        polfile = read.csv(file_name)
        polvector <- c(polvector, polfile[[i]])
    }
    mean(polvector, na.rm=TRUE)
}
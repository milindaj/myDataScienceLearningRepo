corr <- function(directory, threshold = 10) {
  
  ## this function assumes that all the pollution monitoring data
  ## csv files are present under specdata folder under working directory

  ## check input parameters and return display apropriate messages 
  ## in case inputs not as per expectation  
  if (directory != "specdata") {
    print(paste("Directory needs to be 'specdata'. Instead '", directory, "' provided. Returning NULL", sep =""))
    return(NULL)
  }
  
  ## read all the list of files in the directory
  csvList <- dir(directory, full.names = TRUE)
  
  ## loop through the requiredCsvList and read the complete.cases into
  ## a vector
  corrVector = numeric() #intiallizes nobs vector

  for (i in seq_along(csvList)) {
    csvData <- read.csv(csvList[i])
    if (nrow(csvData[complete.cases(csvData), ]) > threshold){
      ##print(paste("****** ", csvList[i]))
      ##bad <- is.na(csvData [, "nitrate"])
      ##goodNData <- csvData [!bad, ] [, "nitrate"]
      goodNData <- csvData [, "nitrate"]

      ##print(str(goodNData))
      ##print("******* print nitrate data ************")      
      ##print(goodNData[1:10])
      
      ##bad <- is.na(csvData [, "sulfate"])
      ##goodSData <- csvData [!bad, ] [, "sulfate"]  
      goodSData <- csvData [, "sulfate"]  
      ##print("******* print sulfate data ************")
      ##print(goodSData[1:10])
      ##print("nrow of goodNData = ", )
      if (length(goodNData) == length(goodSData)) {
        corrVector <- c(corrVector, cor(goodNData, goodSData, use = "na.or.complete"))        
      }
      
    }
  }
  
  return (corrVector)
}
complete <- function(directory, id = 1:332) {
  
  ## this function assumes that all the pollution monitoring data
  ## csv files are present under specdata folder under working directory

  ## check input parameters and return display apropriate messages 
  ## in case inputs not as per expectation  
  if (directory != "specdata") {
    print(paste("Directory needs to be 'specdata'. Instead '", directory, "' provided. Returning NULL", sep =""))
    return(NULL)
  }

  ## check and make sure id is in the range of 1 to 332
  if (min(id) < 1 | max(id) > 332) {
    print("id should be in the range of 1 to 332. Returning NULL")
    return(NULL)
  }
  
  ## read all the list of files in the directory
  csvList <- dir(directory, full.names = TRUE)
  
  ## get the subset of the files identified through id vector
  requiredCsvList <- csvList[id]
  
  ## loop through the requiredCsvList and read the complete.cases into
  ## a vector
  nobsVector = numeric() #intiallizes nobs vector

  for (i in seq_along(requiredCsvList)) {
    csvData <- read.csv(requiredCsvList[i])
    nobsVector <- c(nobsVector, nrow(csvData[complete.cases(csvData), ]))
  }
  
  polDataFrame = data.frame(id = id, nobs = nobsVector) # initialize new data frame
  return (polDataFrame)
}
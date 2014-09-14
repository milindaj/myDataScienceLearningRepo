ponllutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## this function assumes that all the pollution monitoring data
  ## csv files are present under specdata folder under working directory

  ## check input parameters and return display apropriate messages 
  ## in case inputs not as per expectation  
  if (directory != "specdata") {
    print(paste("Directory needs to be 'specdata'. Instead '", directory, "' provided. Returning NULL", sep =""))
    return(NULL)
  }
  
  ## check and make sure pollutant is either 'nitrate' or 'sulfate'
  if (! (pollutant %in% c("nitrate", "sulfate"))) {
    print(paste("Pollutant should be either 'nitrate' or 'sulfate'. Instead '", pollutant, "' provided. Returning NULL", sep =""))
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
  
  ## loop through the requiredCsvList and read the data into dataframe
  ## use rbind to append data from multiple files
  polDataFrame = data.frame() # initialize new data frame
  for (i in seq_along(requiredCsvList)) {
    polDataFrame <- rbind(polDataFrame, read.csv(requiredCsvList[i]))
  }
  
  ## return mean for the selected pollutant and ids
  ## rounding to 3 decimal as per the example result
  return (round(mean(polDataFrame[[ pollutant ]], na.rm = TRUE), 3))
  ## return (round(mean(polDataFrame)
}
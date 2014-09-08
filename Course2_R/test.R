pmean <- function(directory = "specdata", pollutant = "nitrate", id = 1:332) {

	## read all the list of files in the directory
	csvList <- dir(directory, full.names = TRUE)
	
	## get the subset of the files identified through id vector
	requiredCsvList <- csvList[id]

	## loop through the requiredCsvList and print file name
	## for testing purpose
	for (i in seq_along(requiredCsvList )) {
		print(requiredCsvList[i])
	}

	## loop through the requiredCsvList and read the data into dataframe
	## use rbind to append data from multiple files
	polDataFrame = data.frame()
	for (i in seq_along(requiredCsvList)) {
		polDataFrame <- rbind(polDataFrame, read.csv(requiredCsvList[i]))
	}
	
	
	return (round(mean(polDataFrame)
}

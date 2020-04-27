## Libraries
library(openxlsx)
library(stringr)

## Locality list
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)

################################################################################
#
# Get locality level results
#
################################################################################

## Organise indicator results for choropleth mapping
xlsxFiles <- dir(path = "localityResults", pattern = "*.xlsx$")
xlsxFiles <- xlsxFiles[xlsxFiles %in% paste("_", unique(locNames$state), 
                                            ".xlsx", sep = "")]

localityResultsDF <- NULL

for(i in xlsxFiles) {
  ## Get sheet names of current XLSX file
  sheetNames <- getSheetNames(file = paste("localityResults/", i, sep = ""))
  
  ## Get state names from sheets
  state   <- str_remove(str_remove(i, pattern = ".xlsx"), pattern = "_")
  stateID <- unique(locNames$stateID[locNames$state == state])
  
  #indicatorBase <- data.frame(Indicator = currentState$Indicator)
  indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
  indicatorBase <- data.frame(Indicator = indicatorBase$Indicator)
  
  ## Get locality names from sheets
  locality   <- sheetNames[2:length(sheetNames)]
  
  ## Cycle through locality names
  for(j in locality) {
    localityID <- locNames$localityID[locNames$state == state & locNames$locality == j]
    
    ## Read currrent locality sheet
    currentLocality <- read.xlsx(xlsxFile = paste("localityResults/", i, sep = ""), sheet = j)
    currentLocality <- merge(indicatorBase, currentLocality, by = "Indicator", all = TRUE )
    currentLocality <- data.frame(stateID, state, localityID, locality = j, currentLocality)
    
    ## Concatenate data for localities
    localityResultsDF <- data.frame(rbind(localityResultsDF, currentLocality))
  }
}

## Write localityResultsDF as CSV
write.csv(localityResultsDF, "localityResultsDF.csv", row.names = FALSE)

## Tidy up
rm(i, j, sheetNames, state, stateID, locality, localityID, currentLocality)

################################################################################
#
# Get locality level by sex results
#
################################################################################

## Organise indicator results for choropleth mapping
xlsxFiles <- dir(path = "localityResults", pattern = "*.xlsx$")
xlsxFiles <- xlsxFiles[xlsxFiles %in% paste("_", unique(locNames$state), 
                                            ".BySex.xlsx", sep = "")]

localityResultsBySexDF <- NULL

for(i in xlsxFiles) {
  ## Get sheet names of current XLSX file
  sheetNames <- getSheetNames(file = paste("localityResults/", i, sep = ""))
  
  ## Get state names from sheets
  state   <- str_remove(str_remove(i, pattern = ".BySex.xlsx"), pattern = "_")
  stateID <- unique(locNames$stateID[locNames$state == state])
  
  #indicatorBase <- data.frame(Indicator = currentState$Indicator)
  #indicatorBase <- read.csv("indicatorBase.csv")
  #indicatorBase <- data.frame(Indicator = indicatorBase$Indicator)
  
  ## Get locality names from sheets
  locality   <- sheetNames[2:length(sheetNames)]
  
  ## Cycle through locality names
  for(j in locality) {
    localityID <- locNames$localityID[locNames$state == state & locNames$locality == j]
    
    ## Read currrent locality sheet
    currentLocality <- read.xlsx(xlsxFile = paste("localityResults/", i, sep = ""), sheet = j)
    #currentLocality <- merge(indicatorBase, currentLocality, by = "Indicator", all = TRUE )
    currentLocality <- data.frame(stateID, state, localityID, locality = j, currentLocality)
    
    ## Concatenate data for localities
    localityResultsBySexDF <- data.frame(rbind(localityResultsBySexDF, currentLocality))
  }
}

## Write localityResultsDF as CSV
write.csv(localityResultsBySexDF, "localityResultsBySexDF.csv", row.names = FALSE)

## Tidy up
rm(i, j, sheetNames, state, stateID, locality, localityID, currentLocality)

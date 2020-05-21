################################################################################
#
# Post-stratification analysis using the survey package by T Lumley
#
################################################################################
## Libraries
library(survey)
library(openxlsx)
library(stringr)

## Read locality population data
localityPops <- read.csv("localityPops.csv", stringsAsFactors = FALSE)

indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)

allStates <- list()

## Get state names
xlsxFiles <- dir(path = "localityResults", pattern = "*.xlsx$")
xlsxFiles <- xlsxFiles[xlsxFiles %in% paste("_", unique(locNames$state), 
                                            ".xlsx", sep = "")]

stateNames <- str_remove_all(string = xlsxFiles, pattern = ".xlsx|_")

for(i in stateNames) {
  ## Create concatenating list for localities in current state
  localitiesCurrentState <- list()
  ## Get localities for current state results file
  localityNames <- getSheetNames(file = paste("localityResults/_", i, ".xlsx", sep = ""))
  ##
  for(j in localityNames) {
    resultsCurrentLocality <- read.xlsx(xlsxFile = paste("localityResults/_", i, ".xlsx", sep = ""),
                                        sheet = j)
    names(resultsCurrentLocality) <- c("Indicator", "Type", "Estimate", "LCL", "UCL", "sd")
    #resultsCurrentLocality$se <- with(resultsCurrentLocality, sd)
    localitiesCurrentState[[j]] <- subset(resultsCurrentLocality, select = -c(LCL, UCL, sd))
  }
  ##
  allStates[[i]] <- localitiesCurrentState
}


## Accumulator for pooled results
allResults <- NULL
resultsWB <- createWorkbook()

## Cycle through states
#for(i in 1:nrow(indicatorBase))
for(i in names(allStates))
{
  ## current state accumulator for pooled results
  stateResults <- NULL
  ##
  addWorksheet(wb = resultsWB, sheetName = i)
  ## Cycle through rows of indicators
  for(j in 1:nrow(indicatorBase)) 
  {
    ## Get current state
    currentState <- allStates[[i]]
    ## Cycle through localities in current state
    estimates <- standardErrors <- weights <- NULL
    for(k in names(currentState))
    {
      ## Get estimates for current indicator, SE, and population weight in each
      ## locality in current state
      estimates <- c(estimates, allStates[[i]][[k]]$Estimate[j])
      #standardErrors <- c(standardErrors, allStates[[i]][[k]]$se[j])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == i & localityPops$locality == k])
    }
    currentDF <- data.frame(state = i, locality = names(currentState), estimates, weights)
    ## Declare design
    currentDesign <- svydesign(ids = ~1, data = currentDF, weights = weights)
    ##
    weightedEstimate <- svymean(~estimates, design = currentDesign)[1]
    weightedLCL <- confint(svymean(~estimates, design = currentDesign))[1]
    weightedUCL <- confint(svymean(~estimates, design = currentDesign))[2]
    ## Make a results row
    resultRow <- c(weightedEstimate, weightedLCL, weightedUCL)
    stateResults <- rbind(stateResults, resultRow)
  }
  stateResults <- data.frame(indicatorBase[ , 1], stateResults)
  names(stateResults) <- c("Indicator", "Estimate", "LCL", "UCL")
  row.names(stateResults) <- 1:nrow(stateResults)
  writeData(wb = resultsWB, sheet = i, x = stateResults)
  allResults <- rbind(allResults, stateResults)
}

saveWorkbook(wb = resultsWB, file = "_byStatesV2.xlsx", overwrite = TRUE)


## Perform national estimation

## Accumulator for pooled results
nationalResults <- NULL
resultsWB <- createWorkbook()
addWorksheet(wb = resultsWB, sheetName = "national")

## Cycle through states
for(i in 1:nrow(indicatorBase))
{
  estimates <- standardErrors <- weights <- NULL
  for(j in names(allStates)) {
    ##
    currentState <- allStates[[j]]
    ## Cycle through localities in current state
    for(k in names(currentState))
    {
      ## Get estimates for current indicator, SE, and population weight in each
      ## locality in current state
      estimates <- c(estimates, allStates[[j]][[k]]$Estimate[i])
      standardErrors <- c(standardErrors, allStates[[j]][[k]]$se[i])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == j & localityPops$locality == k])
    }
  }
  currentDF <- data.frame(estimates, weights)
  ## Declare design
  currentDesign <- svydesign(ids = ~1, data = currentDF, weights = weights)
  ##
  weightedEstimate <- svymean(~estimates, design = currentDesign)[1]
  weightedLCL <- confint(svymean(~estimates, design = currentDesign))[1]
  weightedUCL <- confint(svymean(~estimates, design = currentDesign))[2]
  ## Make a results row
  resultRow <- c(weightedEstimate, weightedLCL, weightedUCL)
  nationalResults <- rbind(nationalResults, resultRow)
}

nationalResults <- data.frame(indicatorBase[ , 1], indicatorBase[ , 2], nationalResults)
names(nationalResults) <- c("Indicator", "Type", "Estimate", "LCL", "UCL")
row.names(nationalResults) <- 1:nrow(nationalResults)

writeData(wb = resultsWB, sheet = "national", x = nationalResults)

saveWorkbook(wb = resultsWB, file = "_nationalV2.xlsx", overwrite = TRUE)


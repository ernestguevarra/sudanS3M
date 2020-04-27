## Library containing Sudan locality estimates
if(!require("remotes")) install.packages("remotes")
if(!require("sudan")) remotes::install_github("spatialworks/sudan")
library(openxlsx)
library(stringr)

## Locality populations for "post-stratification"
localityPops <- sudan::population_S3M
## Remove three localities not surveyed
localityPops <- localityPops[!localityPops$locality %in% c("Umdoren", 
                                                           "Alburam", 
                                                           "Heban"), ]

indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)

allStates <- list()

## Get state names
## Organise indicator results for choropleth mapping
xlsxFiles <- dir(path = "localityResults", pattern = "*.xlsx$")
xlsxFiles <- xlsxFiles[xlsxFiles %in% paste("_", unique(locNames$state), 
                                            ".xlsx", sep = "")]

stateNames <- str_remove(str_remove(string = xlsxFiles, pattern = ".xlsx"), pattern = "_")

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
    resultsCurrentLocality$se <- with(resultsCurrentLocality, sd)
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
      standardErrors <- c(standardErrors, allStates[[i]][[k]]$se[j])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == i & localityPops$locality == k])
    }
    pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE)))
    pooledLCL <- pooledEstimate - 1.96 * pooledSE
    pooledUCL <- pooledEstimate + 1.96 * pooledSE	
    ## Make a results row
    resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
    stateResults <- rbind(stateResults, resultRow)
  }
  stateResults <- data.frame(indicatorBase[ , 1], stateResults)
  names(stateResults) <- c("Indicator", "Estimator", "LCL", "UCL")
  row.names(stateResults) <- 1:nrow(stateResults)
  writeData(wb = resultsWB, sheet = i, x = stateResults)
  allResults <- rbind(allResults, stateResults)
}

saveWorkbook(wb = resultsWB, file = "_byStates.xlsx", overwrite = TRUE)







## Library containing Sudan locality estimates
if(!require("remotes")) install.packages("remotes")
if(!require("sudan")) remotes::install_github("spatialworks/sudan")
library(openxlsx)

## Read locality results
localityResults <- read.csv("localityResultsDF.csv", stringsAsFactors = FALSE)
names(localityResults) <- c("stateID", "state", "localityID", "locality", 
                            "Indicator", "Type", "Estimate", "LCL", "UCL")


## Process population data to use only states with all localities with population
localityPop <- population_CBS

## Work with localities in Kassala and Khartoum
#subsetPop <- localityPop[localityPop$state %in% c("Kassala", "Khartoum"), ]
subsetResults <- localityResults[localityResults$state %in% c("Kassala", "Khartoum"), ]

##
subStates <- list()
##
for(i in unique(subsetResults$state)) {
  ##
  subLocalities <- list()
  ##
  x <- subset(subsetResults, state == i)
  ##
  for(j in unique(x$locality)) {
    singleLocality <- subset(x, locality == j)
    singleLocality$se <- with(singleLocality, (UCL - LCL) / (2 * 1.96))
    subLocalities[[j]] <- subset(singleLocality, select = -c(LCL, UCL))
  }
  ##
  subStates[[i]] <- subLocalities
}

## Create workbook
resultsWB <- createWorkbook()
## Cycle through rows (indicators)
for(i in  names(subStates)) 
{
  ## Accumulator for pooled results
  results <- NULL
  ## Cycle through states
  for(j in 1:nrow(subStates[[1]][[1]])) 
  {
    ## Cycle through states
    estimates <- standardErrors <- weights <- NULL    
    ## Subset to current state
    x <- subStates[[i]]
    ##
    for(k in names(x)) 
    {
      ## Get estimates for current indicator, SE, and population weight in each
      ## state
      estimates <- c(estimates, x[[k]]$Estimate[j])
      standardErrors <- c(standardErrors, x[[k]]$se[j])
      ## Get weight for state
      weights <- c(weights, localityPop$pop[localityPop$state == i & localityPop$locality == k])
    }
    pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights,
                                                                   na.rm = TRUE)
    pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights,
                                                           na.rm = TRUE)))
    pooledLCL <- pooledEstimate - 1.96 * pooledSE
    pooledUCL <- pooledEstimate + 1.96 * pooledSE	
    ## Make a results row
    resultRow <- c(subStates[[1]][[1]]$Indicator[j], pooledEstimate,
                   pooledLCL, pooledUCL)
    results <- rbind(results, resultRow)
  }
  ## Convert results to a data.frame
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  row.names(results) <- 1:nrow(results)
  
  names(results) <- c("Indicator", "Estimate", "LCL", "UCL")
  for(l in 3:ncol(results))
  {
    results[,l] <- as.numeric(results[,l])
  }
  ##
  addWorksheet(wb = resultsWB, sheetName = i)
  writeData(wb = resultsWB, sheet = i, x = results)
  saveWorkbook(wb = resultsWB, file = "_byStates.xlsx", overwrite = TRUE)
}


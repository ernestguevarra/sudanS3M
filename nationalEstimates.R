## Library to read Excel files
require(openxlsx)

## State populations for "post-stratification"
statePops <- read.xlsx(xlsxFile = "_statePops.xlsx")

## Get results for each state (from prior analysis)
allStates <- list()
stateNames <- getSheetNames(file = "_byStates.xlsx")

for(i in stateNames)
{
  singleState <- read.xlsx(xlsxFile = "_byStates.xlsx", sheet = i)
  names(singleState) <- c("Indicator", "Type", "Estimate", "LCL", "UCL")
  singleState$se <- with(singleState, (UCL - LCL) / (2 * 1.96))
  allStates[[i]] <- subset(singleState, select = -c(LCL, UCL))
}

## Accumulator for pooled results
results <- NULL

## Cycle through rows (indicators)
for(i in 1:nrow(allStates[[1]]))
{
  ## Cycle through states
  estimates <- standardErrors <- weights <- NULL
  for(j in stateNames)
  {
    ## Get estimates for current indicator, SE, and population weight in each
    ## state
    estimates <- c(estimates, allStates[[j]]$Estimate[i])
    standardErrors <- c(standardErrors, allStates[[j]]$se[i])
    ## Get weight for state
    weights <- c(weights, statePops$popCSB[statePops$state == j])
  }
  pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights,
                                                                 na.rm = TRUE)
  pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights,
                                                         na.rm = TRUE)))
  pooledLCL <- pooledEstimate - 1.96 * pooledSE
  pooledUCL <- pooledEstimate + 1.96 * pooledSE	
  ## Make a results row
  resultRow <- c(allStates[[1]]$Indicator[i], pooledEstimate,
                 pooledLCL, pooledUCL)
  results <- rbind(results, resultRow)
}

## Convert results to a data.frame
results <- as.data.frame(results, stringsAsFactors = FALSE)

names(results) <- c("Indicator", "Estimate", "LCL", "UCL")
for(i in 2: ncol(results))
{
  results[,i] <- as.numeric(results[,i])
}

## Save results
write.xlsx(results, file = "_national.xlsx")

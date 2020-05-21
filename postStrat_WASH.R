## Libraries
library(openxlsx)
library(stringr)

## Collate WASH results ########################################################

washDF <-read.xlsx(xlsxFile = "S3M II water source results.xlsx", startRow = 3)

## Extract state results
washState <- washDF[stringr::str_detect(string = washDF$X1, pattern = "State"), ]
washState$X1 <- stringr::str_remove(string = washState$X1, pattern = " State")

## Remove state results
washDF <- washDF[!stringr::str_detect(string = washDF$X1, pattern = "State"), ]
washDF <- washDF[1:(nrow(washDF) - 1), ]

##
xx <- row.names(washState)

state <- c(rep("Northern", 7),
           rep("River Nile", 7),
           rep("Khartoum", 7),
           rep("Al-Gazeera", 8),
           rep("Sinar", 7),
           rep("Blue Nile", 7),
           rep("White Nile", 9),
           rep("Red Sea", 10),
           rep("Kassala", 11),
           rep("Al-Gadarif", 12),
           rep("North Kourdofan", 8),
           rep("South Kourdofan", 14),
           rep("West Kourdofan", 14),
           rep("North Darfur", 17),
           rep("South Darfur", 21),
           rep("East Darfur", 9),
           rep("West Darfur", 8),
           rep("Central Darfur", 9))

washDF <- data.frame(state, washDF, stringsAsFactors = FALSE)

## Extract estimates only

xx <- names(washDF)[stringr::str_detect(string = names(washDF), pattern = "Estimate")]
yy <- names(washDF)[stringr::str_detect(string = names(washDF), pattern = "LCL")]
zz <- names(washDF)[stringr::str_detect(string = names(washDF), pattern = "UCL")]

xx <- washDF[ , c("state", "X1", xx)]
yy <- washDF[ , c("state", "X1", yy)]
zz <- washDF[ , c("state", "X1", zz)]


names(xx) <- c("state", "locality", 
                    "WASH: Uses improved drinking water sources",
                    "WASH: Public pipe network in house",
                    "WASH: Public pipe network in yard",
                    "WASH: Public water pipe",
                    "WASH: Protected ground water",
                    "WASH: Protected well",
                    "WASH: Filtered surface water",
                    "WASH: Trucked from improved sources",
                    "WASH: Bottled water",
                    "WASH: Unfiltered surface water",
                    "WASH: Unprotected well",
                    "WASH: Trucked from unprotected sources",
                    "WASH: Uses basic drinking water sources (within 30 minutes)",
                    "WASH: Public pipe network in house within 30 minutes",
                    "WASH: Public pipe network in yard within 30 minutes",
                    "WASH: Public water pipe within 30 minutes",
                    "WASH: Protected ground water within 30 minutes",
                    "WASH: Protected well within 30 minutes",
                    "WASH: Filtered surface water within 30 minutes",
                    "WASH: Trucked from improved sources within 30 minutes",
                    "WASH: Bottled water within 30 minutes",
                    "WASH: Unfiltered surface water within 30 minutes",
                    "WASH: Unprotected well within 30 minutes",
                    "WASH: Trucked from unprotected sources within 30 minutes")

names(yy) <- c("state", "locality", 
               "WASH: Uses improved drinking water sources",
               "WASH: Public pipe network in house",
               "WASH: Public pipe network in yard",
               "WASH: Public water pipe",
               "WASH: Protected ground water",
               "WASH: Protected well",
               "WASH: Filtered surface water",
               "WASH: Trucked from improved sources",
               "WASH: Bottled water",
               "WASH: Unfiltered surface water",
               "WASH: Unprotected well",
               "WASH: Trucked from unprotected sources",
               "WASH: Uses basic drinking water sources (within 30 minutes)",
               "WASH: Public pipe network in house within 30 minutes",
               "WASH: Public pipe network in yard within 30 minutes",
               "WASH: Public water pipe within 30 minutes",
               "WASH: Protected ground water within 30 minutes",
               "WASH: Protected well within 30 minutes",
               "WASH: Filtered surface water within 30 minutes",
               "WASH: Trucked from improved sources within 30 minutes",
               "WASH: Bottled water within 30 minutes",
               "WASH: Unfiltered surface water within 30 minutes",
               "WASH: Unprotected well within 30 minutes",
               "WASH: Trucked from unprotected sources within 30 minutes")

names(zz) <- c("state", "locality", 
               "WASH: Uses improved drinking water sources",
               "WASH: Public pipe network in house",
               "WASH: Public pipe network in yard",
               "WASH: Public water pipe",
               "WASH: Protected ground water",
               "WASH: Protected well",
               "WASH: Filtered surface water",
               "WASH: Trucked from improved sources",
               "WASH: Bottled water",
               "WASH: Unfiltered surface water",
               "WASH: Unprotected well",
               "WASH: Trucked from unprotected sources",
               "WASH: Uses basic drinking water sources (within 30 minutes)",
               "WASH: Public pipe network in house within 30 minutes",
               "WASH: Public pipe network in yard within 30 minutes",
               "WASH: Public water pipe within 30 minutes",
               "WASH: Protected ground water within 30 minutes",
               "WASH: Protected well within 30 minutes",
               "WASH: Filtered surface water within 30 minutes",
               "WASH: Trucked from improved sources within 30 minutes",
               "WASH: Bottled water within 30 minutes",
               "WASH: Unfiltered surface water within 30 minutes",
               "WASH: Unprotected well within 30 minutes",
               "WASH: Trucked from unprotected sources within 30 minutes")


## Convert to long format dataset
washDF_long <- tidyr::pivot_longer(data = xx, 
                                   cols = `WASH: Uses improved drinking water sources`:`WASH: Trucked from unprotected sources within 30 minutes`,
                                   names_to = "indicator", values_to = "estimate")

yy_long <- tidyr::pivot_longer(data = yy,
                               cols = `WASH: Uses improved drinking water sources`:`WASH: Trucked from unprotected sources within 30 minutes`,
                               names_to = "indicator", values_to = "LCL")

zz_long <- tidyr::pivot_longer(data = zz,
                               cols = `WASH: Uses improved drinking water sources`:`WASH: Trucked from unprotected sources within 30 minutes`,
                               names_to = "indicator", values_to = "UCL")
washDF_long <- data.frame(washDF_long, yy_long[ , "LCL"], zz_long[ , "UCL"], stringsAsFactors = FALSE)


## Locality populations for "post-stratification"
localityPops <- read.csv("localityPops.csv", stringsAsFactors = FALSE)
## Remove three localities not surveyed
localityPops <- localityPops[!localityPops$locality %in% c("Umdoren", 
                                                           "Alburam", 
                                                           "Heban"), ]

indicatorWASH <- data.frame(
                  Indicator = c("WASH: Uses improved drinking water sources",
                                "WASH: Public pipe network in house",
                                "WASH: Public pipe network in yard",
                                "WASH: Public water pipe",
                                "WASH: Protected ground water",
                                "WASH: Protected well",
                                "WASH: Filtered surface water",
                                "WASH: Trucked from improved sources",
                                "WASH: Bottled water",
                                "WASH: Unfiltered surface water",
                                "WASH: Unprotected well",
                                "WASH: Trucked from unprotected sources",
                                "WASH: Uses basic drinking water sources (within 30 minutes)",
                                "WASH: Public pipe network in house within 30 minutes",
                                "WASH: Public pipe network in yard within 30 minutes",
                                "WASH: Public water pipe within 30 minutes",
                                "WASH: Protected ground water within 30 minutes",
                                "WASH: Protected well within 30 minutes",
                                "WASH: Filtered surface water within 30 minutes",
                                "WASH: Trucked from improved sources within 30 minutes",
                                "WASH: Bottled water within 30 minutes",
                                "WASH: Unfiltered surface water within 30 minutes",
                                "WASH: Unprotected well within 30 minutes",
                                "WASH: Trucked from unprotected sources within 30 minutes"),
                  Type = "Proportion",
                  stringsAsFactors = FALSE)

#indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)

allStates <- list()

for(i in washState$X1) {
  ##
  currentState <- subset(washDF_long, state == i)
  ## Create concatenating list for localities in current state
  localitiesCurrentState <- list()
  ## Get localities for current state results file
  localityNames <- currentState$locality
  ##
  for(j in localityNames) {
    resultsCurrentLocality <- subset(currentState, locality == j)
    resultsCurrentLocality$se <- with(resultsCurrentLocality, (UCL - LCL) / (2 * 1.96))
    localitiesCurrentState[[j]] <- subset(resultsCurrentLocality, select = -c(LCL, UCL))
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
  for(j in 1:nrow(indicatorWASH)) 
  {
    ## Get current state
    currentState <- allStates[[i]]
    ## Cycle through localities in current state
    estimates <- standardErrors <- weights <- NULL
    for(k in names(currentState))
    {
      ## Get estimates for current indicator, SE, and population weight in each
      ## locality in current state
      estimates <- c(estimates, allStates[[i]][[k]]$estimate[j])
      standardErrors <- c(standardErrors, allStates[[i]][[k]]$se[j])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == i & localityPops$locality == k])
    }
    pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE), na.rm = TRUE))
    
    ## Correct for LCL less than 0
    pooledLCL <- pooledEstimate - 1.96 * pooledSE
    pooledLCL <- ifelse(indicatorWASH$Type[j] == "Proportion" & pooledLCL < 0 & pooledEstimate > 0.0100, 0.0100, pooledLCL)
    pooledLCL <- ifelse(indicatorWASH$Type[j] == "Proportion" & pooledLCL < 0 & pooledEstimate >= 0 & pooledEstimate < 0.0100, 0, pooledLCL)

    ## Correct for UCL greater than 1
    pooledUCL <- pooledEstimate + 1.96 * pooledSE
    pooledUCL <- ifelse(indicatorWASH$Type[j] == "Proportion" & pooledUCL > 100, 99.99, pooledUCL)

    ## Convert proportion estimates to percentages and round off estimates to 2 decimal places
    #pooledEstimate <- ifelse(indicatorWASH$Type[j] == "Proportion", pooledEstimate * 100, pooledEstimate)
    pooledEstimate <- round(pooledEstimate, digits = 2)
    #pooledLCL <- ifelse(indicatorWASH$Type[j] == "Proportion", pooledLCL * 100, pooledLCL)
    pooledLCL <- round(pooledLCL, digits = 2)    
    #pooledUCL <- ifelse(indicatorWASH$Type[j] == "Proportion", pooledUCL * 100, pooledUCL)
    pooledUCL <- round(pooledUCL, digits = 2)    
    
    ## Make a results row
    resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
    stateResults <- rbind(stateResults, resultRow)
  }
  stateResults <- data.frame(indicatorWASH[ , 1], indicatorWASH[ , 2], stateResults)
  names(stateResults) <- c("Indicator", "Type", "Estimator", "LCL", "UCL")
  row.names(stateResults) <- 1:nrow(stateResults)
  writeData(wb = resultsWB, sheet = i, x = stateResults)
  allResults <- rbind(allResults, stateResults)
}

saveWorkbook(wb = resultsWB, file = "_byStates.WASH.xlsx", overwrite = TRUE)

######################### Perform national estimation ##########################

## Accumulator for pooled results
nationalResults <- NULL
resultsWB <- createWorkbook()
addWorksheet(wb = resultsWB, sheetName = "national")

## Cycle through states
for(i in 1:nrow(indicatorWASH))
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
      estimates <- c(estimates, allStates[[j]][[k]]$estimate[i])
      standardErrors <- c(standardErrors, allStates[[j]][[k]]$se[i])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == j & localityPops$locality == k])
    }
  }
  pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE), na.rm = TRUE))
    
  ## Correct for LCL less than 0
  pooledLCL <- pooledEstimate - 1.96 * pooledSE
  pooledLCL <- ifelse(indicatorWASH$Type[i] == "Proportion" & pooledLCL < 0 & pooledEstimate > 0.0100, 0.0100, pooledLCL)
  pooledLCL <- ifelse(indicatorWASH$Type[i] == "Proportion" & pooledLCL < 0 & pooledEstimate >= 0 & pooledEstimate < 0.0100, 0, pooledLCL)
    
  ## Correct for UCL greater than 1
  pooledUCL <- pooledEstimate + 1.96 * pooledSE
  pooledUCL <- ifelse(indicatorWASH$Type[i] == "Proportion" & pooledUCL > 100, 99.99, pooledUCL)
    
  ## Convert proportion estimates to percentages and round off estimates to 2 decimal places
  #pooledEstimate <- ifelse(indicatorWASH$Type[i] == "Proportion", pooledEstimate * 100, pooledEstimate)
  pooledEstimate <- round(pooledEstimate, digits = 2)
  #pooledLCL <- ifelse(indicatorWASH$Type[i] == "Proportion", pooledLCL * 100, pooledLCL)
  pooledLCL <- round(pooledLCL, digits = 2)    
  #pooledUCL <- ifelse(indicatorWASH$Type[i] == "Proportion", pooledUCL * 100, pooledUCL)
  pooledUCL <- round(pooledUCL, digits = 2)
  
  ## Make a results row
  resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
  nationalResults <- rbind(nationalResults, resultRow)
}

nationalResults <- data.frame(indicatorWASH[ , 1], indicatorWASH[ , 2], nationalResults)
names(nationalResults) <- c("Indicator", "Type", "Estimator", "LCL", "UCL")
row.names(nationalResults) <- 1:nrow(nationalResults)

writeData(wb = resultsWB, sheet = "national", x = nationalResults)

saveWorkbook(wb = resultsWB, file = "_national.WASH.xlsx", overwrite = TRUE)


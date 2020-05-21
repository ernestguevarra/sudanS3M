## Prepare the workspace #######################################################

## Libraries 
library(openxlsx)
library(stringr)
library(ggplot2)

## Load UNICEF palette
unicef_blue      <- "#1CABE2"
unicef_darkgreen <- "#00833D"
unicef_green     <- "#80BD41"
unicef_yellow    <- "#FFC20E"
unicef_orange    <- "#F26A21"
unicef_red       <- "#E2231A"
unicef_darkred   <- "#961A49" 
unicef_purple    <- "#6A1E74"
unicef_warmgrey  <- "#D8D1C9"
unicef_coolgrey  <- "#777779"
unicef_black     <- "#2D2926"
unicef_darkblue  <- "#374EA2"

## Simple ggplot theme settings
themeSettings <- theme_bw() + 
  theme(panel.border = element_rect(linetype = 1,
                                    size = 1,
                                    colour = "gray50"),
        panel.grid.major.y = element_line(linetype = 1, 
                                          size = 0.1,
                                          colour = "gray90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "gray50",
                                        fill = "gray70"),
        strip.text = element_text(colour = "white", size = 10),
        legend.key = element_rect(linetype = 0),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_line(colour = "gray50", size = 0.5))

## UNICEF ggplot theme settings
themeUNICEF <- theme_bw() +
  theme(panel.border = element_rect(colour = unicef_blue,
                                    size = 0.5),
        panel.grid.major = element_line(linetype = 1, 
                                        size = 0.1,
                                        colour = unicef_warmgrey),
        panel.grid.minor = element_line(linetype = 0),
        strip.background = element_rect(colour = unicef_blue,
                                        fill = unicef_blue),
        strip.text = element_text(colour = "white"),
        legend.key = element_rect(linetype = 0),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_line(colour = unicef_blue, size = 0.5))

## Load supporting files #######################################################

## Indicators template
indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
indicatorBaseEdu <- read.csv("educationIndicatorBase.csv", stringsAsFactors = FALSE)
indicatorBaseWASH <- read.csv("indicatorBaseWASH.csv", stringsAsFactors = FALSE)

## All localities steering file
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)
## Create state steering file
stateSteer <- unique(locNames[ , c("stateID", "state")])
row.names(stateSteer) <- 1:nrow(stateSteer)

## Create all localities and all indicators dataset ############################

## Create all states and all indicators dataset ################################

## Read per state results and concatenate
allStates <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "resultsS3M/_byStates.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "resultsS3M/_byStates.xlsx", sheet = i)
  
  resultsCurrentState <- resultsCurrentState[c(1:237, 240:243), ]
  
  ## Add column for state name
  resultsCurrentState <- data.frame(stateID = stateSteer$stateID[stateSteer$state == i],
                                    state = i,
                                    resultsCurrentState, stringsAsFactors = FALSE)
  
  ## Rename columns in current state results
  names(resultsCurrentState) <- c("stateID", "state", "Indicator", 
                                  "Type", "Estimate", "X95..LCL", "X95..UCL")

  ## Concatenate results
  allStates <- rbind(allStates, resultsCurrentState)
}

## Save allStates as CSV
write.csv(x = allStates, file = "resultsS3M/stateResults.csv", row.names = FALSE)

## Create all states and all education indicators dataset ######################

## Read per state results and concatenate
allStatesEdu <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "resultsS3M/_byStates.bySex.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "resultsS3M/_byStates.bySex.xlsx", sheet = i)
  
  ## Add column for state name
  resultsCurrentState <- data.frame(stateID = stateSteer$stateID[stateSteer$state == i],
                                    state = i,
                                    resultsCurrentState, stringsAsFactors = FALSE)
  
  ## Rename columns in current state results
  names(resultsCurrentState) <- c("stateID", "state", "Indicator", 
                                  "Type", "Estimate", "X95..LCL", "X95..UCL")
  
  ## Concatenate results
  allStatesEdu <- rbind(allStatesEdu, resultsCurrentState)
}

## Save allStatesEdu as CSV
write.csv(x = allStatesEdu, file = "resultsS3M/stateResultsEdu.csv", row.names = FALSE)

## Create all states and all WASH indicators dataset ###########################

## Read per state results and concatenate
allStatesWASH <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "resultsS3M/_byStates.WASH.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "resultsS3M/_byStates.WASH.xlsx", sheet = i)
  
  ## Add column for state name
  resultsCurrentState <- data.frame(stateID = stateSteer$stateID[stateSteer$state == i],
                                    state = i,
                                    resultsCurrentState, stringsAsFactors = FALSE)
  
  ## Rename columns in current state results
  names(resultsCurrentState) <- c("stateID", "state", "Indicator", 
                                  "Type", "Estimate", "X95..LCL", "X95..UCL")
  
  ## Concatenate results
  allStatesWASH <- rbind(allStatesWASH, resultsCurrentState)
}

## Save allStatesEdu as CSV
write.csv(x = allStatesWASH, file = "resultsS3M/stateResultsWASH.csv", row.names = FALSE)

## Create plots ################################################################


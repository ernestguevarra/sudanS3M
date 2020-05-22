## Prepare the workspace #######################################################

## Libraries 
library(openxlsx)
library(stringr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggrepel)

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
  theme(plot.title = element_text(size = 20, colour = "gray50"),
        plot.subtitle = element_text(size = 16, colour = "gray70"),
        panel.border = element_rect(linetype = 1,
                                    size = 1,
                                    colour = "gray50"),
        panel.grid.major.y = element_line(linetype = 1, 
                                          size = 0.1,
                                          colour = "gray90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "gray50",
                                        fill = "gray70"),
        strip.text = element_text(colour = "white", size = 12),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(x = 12, units = "points"),
        legend.spacing = unit(x = 4, units = "points"),
        legend.text = element_text(size =  12),
        axis.line.x = element_line(colour = "gray50", size = 0.75),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, colour = "gray70"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, colour = "gray70"),
        axis.ticks = element_line(colour = "gray50", size = 0.5))

## UNICEF ggplot theme settings
themeUNICEF <- theme_bw() +
  theme(plot.title = element_text(size = 20, colour = unicef_darkblue),
        plot.subtitle = element_text(size = 16, colour = unicef_blue),
        panel.border = element_rect(colour = unicef_darkblue,
                                    size = 0.5),
        panel.grid.major = element_line(linetype = 1, 
                                        size = 0.1,
                                        colour = unicef_warmgrey),
        panel.grid.minor = element_line(linetype = 0),
        strip.background = element_rect(colour = unicef_darkblue,
                                        fill = unicef_darkblue),
        strip.text = element_text(colour = "white", size = 12),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(x = 12, units = "points"),
        legend.spacing = unit(x = 4, units = "points"),
        legend.text = element_text(size =  12),
        axis.line.x = element_line(colour = unicef_darkblue, size = 0.75),
        axis.text.x = element_text(size = 12, colour = unicef_darkblue),
        axis.title.x = element_text(size = 16, colour = unicef_blue),
        axis.text.y = element_text(size = 12, colour = unicef_darkblue),
        axis.title.y = element_text(size = 16, colour = unicef_blue),
        axis.ticks = element_line(colour = unicef_darkblue, size = 0.5))

## Functions ###################################################################

get_outlier <- function(x) {
  ## 
  outlier <- x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)
  outlierClass <- ifelse(x > quantile(x, 0.75) + 1.5 * IQR(x), "high",
                         ifelse(x < quantile(x, 0.25) - 1.5 * IQR(x), "low", "in range"))
  ##
  outlier <- data.frame(outlier, outlierClass, stringsAsFactors = FALSE)
  ##
  return(outlier)
}

## Load supporting files #######################################################

## Indicators template
indicatorBase <- read.csv("indicatorBase.csv", stringsAsFactors = FALSE)
indicatorBaseEdu <- read.csv("educationIndicatorBase.csv", stringsAsFactors = FALSE)
indicatorBaseWASH <- read.csv("indicatorBaseWASH.csv", stringsAsFactors = FALSE)

## Indicators list
indicatorList <- read.csv("indicatorList.csv", stringsAsFactors = FALSE)

## All localities steering file
locNames <- read.csv("locNames.csv", stringsAsFactors = FALSE)
## Create state steering file
stateSteer <- unique(locNames[ , c("stateID", "state")])
row.names(stateSteer) <- 1:nrow(stateSteer)

## Read locality results data
localityResults <- read.csv("localityResultsDF.csv", stringsAsFactors = FALSE)
localityResultsEdu <- read.csv("localityResultsBySexDF.csv", stringsAsFactors = FALSE)

## Create all states and all indicators dataset ################################

## Read per state results and concatenate
allStates <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "_byStates.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "_byStates.xlsx", sheet = i)
  
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
write.csv(x = allStates, file = "stateResults.csv", row.names = FALSE)

## Create all states and all education indicators dataset ######################

## Read per state results and concatenate
allStatesEdu <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "_byStates.bySex.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "_byStates.bySex.xlsx", sheet = i)
  
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
write.csv(x = allStatesEdu, file = "stateResultsEdu.csv", row.names = FALSE)

## Create all states and all WASH indicators dataset ###########################

## Read per state results and concatenate
allStatesWASH <- data.frame()

## Get state names
stateNames <- getSheetNames(file = "_byStates.WASH.xlsx")

for(i in stateNames) {
  ## Read worksheet for current state
  resultsCurrentState <- read.xlsx(xlsxFile = "_byStates.WASH.xlsx", sheet = i)
  
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
write.csv(x = allStatesWASH, file = "stateResultsWASH.csv", row.names = FALSE)

## Create plots: ANC coverage ##################################################

## Get indicator names/labels
varLabel <- indicatorList$varLabel[indicatorList$varSet == "ANC coverage"]

## Gestational age (months) at first ANC visit
x <- localityResults[localityResults$Indicator == varLabel[1], ]

## Simple theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%
  ggplot(mapping = aes(x = state, y = Estimate)) +
  geom_boxplot(width = 0.5, size = 0.75, colour = "gray50", outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text(mapping = aes(label = outlierLabel),
            size = 3,
            na.rm = TRUE, 
            vjust = 1.5) +
  scale_colour_manual(name = " ", values = c("darkgreen", "blue", "red")) +
  annotate(geom = "segment", color = "gray50", 
           y = -Inf, yend = Inf, x = Inf, xend = Inf, 
           size = 2) +
  labs(title = "Mean gestational age (months) at first ANC visit",
       subtitle = "Localities by state",
       x = NULL, y = "Gestational age (months)") +
  coord_flip() +
  themeSettings +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save plot
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[1]], ".png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300)

## UNICEF theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%
  ggplot(mapping = aes(x = state, y = Estimate)) +
  geom_boxplot(width = 0.5, colour = unicef_darkblue, size = 0.75,
               outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text(mapping = aes(label = outlierLabel),
            size = 3,
            na.rm = TRUE, 
            vjust = 1.5) +
  scale_colour_manual(name = " ", values = c(unicef_darkgreen, 
                                             unicef_blue, 
                                             unicef_darkred)) +
  annotate(geom = "segment", color = unicef_darkblue, 
           y = -Inf, yend = Inf, 
           x = Inf, xend = Inf, 
           size = 2) +
  labs(title = "Mean gestational age (months) at first ANC visit",
       subtitle = "Localities by state",
       x = NULL, y = "Gestational age (months)") +
  coord_flip() +
  themeUNICEF +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[1]], "_un.png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300) 

## Number of ANC visits during most recent pregnancy
x <- localityResults[localityResults$Indicator == varLabel[2], ]

## Simple theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%  
  ggplot(mapping = aes(x = state, y = Estimate)) +
  geom_boxplot(width = 0.5, colour = "gray50", 
               size = 0.75, outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text(mapping = aes(label = outlierLabel),
            size = 3,
            na.rm = TRUE, 
            vjust = 1.5) +
  scale_colour_manual(name = " ", values = c("darkgreen", "blue", "red")) +
  annotate(geom = "segment", color = "gray50", 
           y = -Inf, yend = Inf, 
           x = Inf, xend = Inf, 
           size = 1) +
  labs(title = "Mean number of ANC visits during most\nrecent pregnancy",
       subtitle = "Localities by state",
       x = NULL, y = "No. of ANC visits") +
  coord_flip() +
  themeSettings +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[2]], ".png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300)

## UNICEF theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%
  ggplot(mapping = aes(x = state, y = Estimate)) +
  geom_boxplot(width = 0.5, colour = unicef_darkblue, size = 0.75,
               outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text(mapping = aes(label = outlierLabel),
            size = 3,
            na.rm = TRUE, 
            vjust = 1.5) +
  scale_colour_manual(name = " ", values = c(unicef_darkgreen, 
                                             unicef_blue, 
                                             unicef_darkred)) +
  annotate(geom = "segment", color = unicef_darkblue, 
           y = -Inf, yend = Inf, 
           x = Inf, xend = Inf, 
           size = 1) +
  labs(title = "Mean number of ANC visits during most\nrecent pregnancy",
       subtitle = "Localities by state",
       x = NULL, y = "No. of ANC visits") +
  coord_flip() +
  themeUNICEF +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[2]], "_un.png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300)

## Did not attend ANC during most recent pregnancy
x <- localityResults[localityResults$Indicator %in% varLabel[3], ]

## Simple theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%  
  ggplot(mapping = aes(x = state, y = Estimate * 100)) +
  geom_boxplot(width = 0.5, colour = "gray50", 
               size = 0.75, outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text_repel(mapping = aes(label = outlierLabel), size = 3, na.rm = TRUE) +
  scale_colour_manual(name = " ", values = rev(c("darkgreen", "blue", "red"))) +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(from = 0, to = 100, by = 10)) +
  annotate(geom = "segment", color = "gray50", 
           y = -Inf, yend = Inf, 
           x = Inf, xend = Inf, 
           size = 1) +
  labs(title = "Did not attend ANC during most recent pregnancy",
       subtitle = "Localities by state",
       x = NULL, y = "%") +
  coord_flip() +
  themeSettings +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[3]], ".png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300)

## UNICEF theme
x %>%
  group_by(state) %>%
  mutate(outlierLabel = ifelse(get_outlier(Estimate)$outlier, locality, NA),
         outlierClass = get_outlier(Estimate)$outlierClass) %>%
  ggplot(mapping = aes(x = state, y = Estimate * 100)) +
  geom_boxplot(width = 0.5, colour = unicef_darkblue, size = 0.75,
               outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = outlierClass),
              width = 0.05, size = 2, shape = 19, alpha = 0.7,
              na.rm = TRUE) +
  geom_text_repel(mapping = aes(label = outlierLabel), size = 3, na.rm = TRUE) +
  scale_colour_manual(name = " ", values = rev(c(unicef_darkgreen, 
                                             unicef_blue, 
                                             unicef_darkred))) +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(from = 0, to = 100, by = 10)) +
  annotate(geom = "segment", color = unicef_darkblue, 
           y = -Inf, yend = Inf, 
           x = Inf, xend = Inf, 
           size = 1) +
  labs(title = "Did not attend ANC during most recent pregnancy",
       subtitle = "Localities by state",
       x = NULL, y = "%") +
  coord_flip() +
  themeUNICEF +
  theme(panel.border = element_blank(),
        legend.position = "top")

## Save
ggsave(filename = paste(indicatorList$varNames[indicatorList$varLabel == varLabel[3]], "_un.png", sep = ""),
       path = "figures",
       device = "png", width = 20, height = 35, units = "cm", dpi = 300)


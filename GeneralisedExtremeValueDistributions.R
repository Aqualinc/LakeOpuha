#Script to generate regime threshold files that can be used in the Lake Simulator

GEVParameterGenerator <- function(InputDataFileName = "LevelInflowSnow1998-2017.csv") {

if (!require(zoo)) install.packages('zoo'); library(zoo)
#if (!require(knitr)) install.packages('knitr'); library(knitr)
if (!require(lubridate)) install.packages('lubridate'); library(lubridate) #Has some useful date functions
if (!require(EnvStats)) install.packages('EnvStats'); library(EnvStats)    #has the generalized extreme value distribution fitting

LevelInflowSnowData <- read.csv(InputDataFileName, stringsAsFactors = FALSE)

#Convert inflow data from l/s to cumeces
LevelInflowSnowData$Lake.Inflows <- LevelInflowSnowData$Lake.Inflows/1000

DaysOfYear <- seq(1,365)

#Calculate 30 day averages for the inflows and snow storage, but just previous-7-day-average for the lake levels
LevelInflowSnowData$Date <- as.Date(LevelInflowSnowData$Date, format = "%d/%m/%Y")
LevelInflowSnowData$JulianDay <- as.numeric(format(LevelInflowSnowData$Date, "%j"))
f30 <- rep(1/30, 30)    #30 day filter

#Find the inflow and snow storage 30 day averages (IS stands for 'I'nflow and 'S'now storage)
IS30DayAverage <- stats::filter(LevelInflowSnowData[,c("Lake.Inflows","Snow.Storage")], f30, sides=1)

colnames(IS30DayAverage) <- c("Inflows","Snow")

#Create a list of 20 values (one for each year) for each day of the year, for each parameter
IS_DOY_Value <- lapply(IS30DayAverage, function(ParameterOfInterest) {
  #Work through each day of the year
  IS_DOY_ValueSingleParameter <- sapply(DaysOfYear, function(DayOfInterest) {
    DOY_Values <- ParameterOfInterest[which(as.numeric(format(LevelInflowSnowData$Date,"%j")) == DayOfInterest)]
    
    #Get rid of any NA values
    DOY_Values <- DOY_Values[!is.na(DOY_Values)]
  })
})

#Fit a generalized extreme value distribution to the previous-multi-day-averages for each day of the year, and from this parameterised distribution read-off the percentiles of interest to provide daily thresholds for level 1 and level 2.

#Create a list of the GEV parameters for each environmental parameter (i.e. inflows and snow storage) and day of the year
GEVParameters <- lapply(IS_DOY_Value, function(SingleParameterData) {
  #work through each day of the year
  GEVParametersSingleParameter <- lapply(DaysOfYear, function(DayOfInterest) {
    
    DataOfInterest <- SingleParameterData[[DayOfInterest]]

    #Find the parameters of the distribution
    #Sometimes the "mle" method fails, so under those conditions, use the "pwme" option.
    DistParameters <- tryCatch({
      egevd(DataOfInterest,method="mle")
    },warning = function(warning) {
      egevd(DataOfInterest,method="pwme")
    }, 
    error = function(error) {
      egevd(DataOfInterest,method="pwme")
    })
    
    #Determine the values for specific probabilities
    #SingleParameterThresholds <- qgevd(ThresholdPercentiles,DistParameters$parameters['location'],
    #                    DistParameters$parameters['scale'],DistParameters$parameters['shape'])
    return(DistParameters)
  })
  return(GEVParametersSingleParameter)
})

return(list(GEVParameters=GEVParameters,AveragedObs = IS_DOY_Value))
}


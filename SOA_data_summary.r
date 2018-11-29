### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis/Intermediate"
setwd(wd)


###################
#setup
###################

metrics <- c("Cruise_tourism", "Domestic_tourism", "International_tourism", "Population", "Shipping_distance", "Hunters", "Fishing", "Oilandgas_wells")
#metrics <- c("Oilandgas_wells")

#expressed as annual growth rate over start years value. 
#annual growth rate = (final/start)^(1/nyrs) -1
growthfun <- function(data, ys, yf) {
                if(length(data$year %in% c(ys:yf))<2) {
                  anngrowth <- NA
                } else {
                  if(!(ys %in% data$year)){ # is missing, find the closest start year
                    ys <- data$year[which.min(abs(data$year - ys))]
                  } 
                  if (!(yf %in% data$year)){ #if missing, find the closest end year
                    yf <- data$year[which.min(abs(data$year[!is.na(data$value)] - yf))]
                  }
                  s = data$value[data$year==ys]
                  f = data$value[data$year==yf]
                  nyr = yf - ys
                  anngrowth <- (f/s)^(1/nyr) - 1
                }
                  return( list(ys, yf, anngrowth))
              }


###################
#Main Processing
###################

for(currm in metrics) {
  print(currm)
  #load file
  dat <- read.csv(paste0(currm, "_long.csv"), header=TRUE, fileEncoding = "UTF-8-BOM")
  #drop rows with NAs
  dat <- dat[!is.na(dat$value), ] %>% droplevels()
  #dat <- dat[dat$value!=0, ] %>% droplevels()
  
  #summarise
  uniquevals <- unique(dat[, c("Country", "Region", "Metric")])
  
  currsum <- lapply(1:nrow(uniquevals), function(i){
    #currsum <- c()
    #for (i in 1:nrow(uniquevals)){
    subdf <- dat %>% filter(Country==uniquevals$Country[i] & 
                            Region==uniquevals$Region[i] & 
                            Metric==uniquevals$Metric[i]) %>% droplevels()
    #select years with data
    start_yr=try(min( subdf$year[which(subdf$value > 0)] ))
    end_yr=try(max( subdf$year[which(subdf$value > 0)] ))
      #if the value is 0 for all years select the first and last year
      if(is.infinite(start_yr)) {
       start_yr=min(subdf$year) 
        end_yr=max( subdf$year)
      }
    
    #calculate intensity 
    if(currm=="Oilandgas_wells"){
      intensity <- sum(subdf$value[subdf$year %in% c(1960:2017)], na.rm=TRUE)
    } else {
      intensity <- mean(subdf$value[subdf$year %in% c(2012:2017)], na.rm=TRUE)
    }
    #bind summary
    currdf <- data.frame(uniquevals[i,], 
                 growthfun(subdf, start_yr, end_yr),
                 growthfun(subdf, 1997, 2002),
                 growthfun(subdf, 2002, 2007),
                 growthfun(subdf, 2007, 2012),
                 growthfun(subdf, 2012, 2017),
                 intensity)
    names(currdf) <- c("Country", "Region", "Metric", "Start_yr", "End_yr", "AnnGrowth_start_end", 
                       "Start_97", "End_02", "AnnGrowth_97_02", 
                       "Start_02", "End_07", "AnnGrowth_02_07", 
                       "Start_07", "End_12", "AnnGrowth_07_12", 
                       "Start_12", "End_17", "AnnGrowth_12_17", 
                       "Intensity")
    #currsum <- rbind(currsum, currdf)
    #}
            return(currdf)
    })
  df <- do.call(rbind, currsum)
write.csv(df, paste0(currm, "_summarystats.csv"), row.names=FALSE)
}

################
#exploring shipping mystery
currm="vesselweight"

#load file
dat <- read.csv("D:/Box Sync/Arctic/Data/Transport and accessibility/Shipping/Havbase_arktis/ArcticShipping_byregion_vesselweight_year.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
names(dat)[3:4] <- c("year", "value")
#summarise
uniquevals <- unique(dat[, c("shipweight", "region")])

currsum <- lapply(1:nrow(uniquevals), function(i){
  #currsum <- c()
  #for (i in 1:nrow(uniquevals)){
  subdf <- dat %>% filter(shipweight==uniquevals$shipweight[i] & region==uniquevals$region[i]) %>% droplevels()
  intensity <- mean(subdf$value[subdf$year %in% c(2012:2017)], na.rm=TRUE)
  #bind summary
  currdf <- data.frame(uniquevals[i,], 
                       growthfun(subdf, 2012, 2017),
                       intensity)
  names(currdf) <- c("region", "vesselweight",  
                     "Start_12", "End_17", "AnnGrowth_12_17", 
                     "Intensity")
  #currsum <- rbind(currsum, currdf)
  #}
  return(currdf)
})
df <- do.call(rbind, currsum)
write.csv(df, paste0(currm, "_summarystats.csv"), row.names=FALSE)

###
currm="vesselweight"
#summarise
uniquevals <- unique(dat[, c("shipweight")])

currsum <- lapply(1:length(uniquevals), function(i){
  #currsum <- c()
  #for (i in 1:nrow(uniquevals)){
  subdf <- dat %>% filter(shipweight==uniquevals[i] & region=="Total") %>% droplevels()
  intensity <- mean(subdf$value[subdf$year %in% c(2012:2017)], na.rm=TRUE)
  #bind summary
  currdf <- data.frame(uniquevals[i], 
                       growthfun(subdf, 2012, 2017),
                       intensity)
  names(currdf) <- c("vesselweight",  
                     "Start_12", "End_17", "AnnGrowth_12_17", 
                     "Intensity")
  #currsum <- rbind(currsum, currdf)
  #}
  return(currdf)
})
df <- do.call(rbind, currsum)
write.csv(df, paste0(currm, "_summarystats.csv"), row.names=FALSE)

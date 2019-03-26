
### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)
require(sf)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis"
setwd(wd)

#Ship dist
shipdist <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_distancesailed_LME", range="A1:J81")
shipdist$Country <- as.factor(shipdist$Country)
shipdist$Region <- as.factor(shipdist$region)
shipdist$Metric <- as.factor(shipdist$ShipCat)
shipdist_long <- gather(shipdist, key=year, value=value, "2012":"2018")
write_excel_csv(shipdist_long, "Intermediate/Shipping_distance_LME_long.csv")

#Ship trafficwork
shiptraff <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_trafficwork_LME", range="A1:H3676")
shiptraff <- shiptraff[shiptraff$Year < 2018, c("Country", "region", "shipweight", "ShipCat", "Year", "traffic_work_gTnm")]
names(shiptraff) <- c("Country", "Region", "Shipweight", "ShipCat", "year", "value")
write_excel_csv(shiptraff, "Intermediate/Shipping_trafficwork_LME_long.csv")

#Ship vol
shipvol <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_voltransported", range="A1:AB26")
shipvol$Country <- as.factor(shipvol$Country)
shipvol$Metric <- as.factor(shipvol$Cargo)
shipvol$Region <- as.factor(shipvol$Region)
shipvol_long <- gather(shipvol, key=year, value=value, "1991":"2016")
write_excel_csv(shipvol_long, "Intermediate/Shipping_volume_long.csv")

#population
popn <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="population", range="A1:AF54")
popn$Country <- as.factor(popn$Country)
popn$Region <- as.factor(popn$Region)
popn$Metric <- as.factor(popn$Metric)
popn_long <- gather(popn, key=year, value=value, "1990":"2018")
write_excel_csv(popn_long, "Intermediate/Population_long.csv")

#tourism
tur_cruise <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_cruise", range="A1:AF18")
tur_cruise$Country <- as.factor(tur_cruise$Country)
tur_cruise$Region <- as.factor(tur_cruise$Region)
tur_cruise$Metric <- as.factor(tur_cruise$Metric)
tur_cruise_long <- gather(tur_cruise, key=year, value=value, "1990":"2018")
write_excel_csv(tur_cruise_long, "Intermediate/Cruise_tourism_long.csv")

tur_domestic <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_domestic", range="A1:U25")
tur_domestic$Country <- as.factor(tur_domestic$Country)
tur_domestic$Region <- as.factor(tur_domestic$Region)
tur_domestic$Metric <- as.factor(tur_domestic$Metric)
tur_domestic_long <- gather(tur_domestic, key=year, value=value, "2000":"2017")
write_excel_csv(tur_domestic_long, "Intermediate/Domestic_tourism_long.csv")

tur_int <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_international", range="A1:U25")
tur_int$Country <- as.factor(tur_int$Country)
tur_int$Region <- as.factor(tur_int$Region)
tur_int$Metric <- as.factor(tur_int$Metric)
tur_int_long <- gather(tur_int, key=year, value=value, "2000":"2017")
write_excel_csv(tur_int_long, "Intermediate/International_tourism_long.csv")

tur_all <- rbind(tur_cruise_long, tur_domestic_long, tur_int_long)
tur_all$Touristtype <- rep(c("Cruise", "Domestic", "International"), times=c(nrow(tur_cruise_long), nrow(tur_domestic_long), nrow(tur_int_long)))
write_excel_csv(tur_all, "Intermediate/All_tourism_long.csv")


#Oil and gas
#wells
wells <- read_sf(dsn="D:/Box Sync/Arctic/Data/Landuse/Oil_gas/Arctic_amap", layer="Wells_oil_and_gas_arctic_nocanadanwtoffshore_amap_plusregion" )
wells <- wells %>% st_set_geometry(NULL)
names(wells)[7:10] <- c("Status", "Type", "Start_yr", "End_yr")
wells$Country <- as.factor(wells$Country)
wells$Region <- as.factor(wells$Region)
wells$Metric <- as.factor(wells$Type)
#fix year where blank or >2018
wells$year <- wells$Start_yr
wells$year[wells$Start_yr==0] <- wells$End_yr[wells$Start_yr==0]
wells$year[wells$Start_yr>2018] <- wells$End_yr[wells$Start_yr>2018]
wells_long <- wells %>% group_by(Country, Region, Metric, year) %>% summarise(value=n())
write_excel_csv(wells_long, "Intermediate/Oilandgas_wells_long.csv")

#mining
minesex <- read.csv("D:/Box Sync/Arctic/Data/Landuse/Mining/Ojas_mining_database_2018/processed/Mining_nmines_exploration_byyear.csv", fileEncoding = "UTF-8-BOM")
minesex$Metric <- "Exploration"
minesex_long <- gather(minesex, key=year, value=value, "X1962":"X2017")
minesop <- read.csv("D:/Box Sync/Arctic/Data/Landuse/Mining/Ojas_mining_database_2018/processed/Mining_nmines_operational_byyear.csv", fileEncoding = "UTF-8-BOM")
minesop$Metric <- "Operation"
minesop_long <- gather(minesop, key=year, value=value, "X1900":"X2017")
mines <- rbind(minesex_long, minesop_long)
mines <- mines %>% rowwise %>% mutate(year=as.integer(str_split(year, "X")[[1]][2]))
write_excel_csv(mines, "Intermediate/Mining_long.csv")

#Hunting
#number of hunters
hunters <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="Hunters", range="A1:AK45")
hunters$Country <- as.factor(hunters$Country)
hunters$Region <- as.factor(hunters$Region)
hunters$Metric <- as.factor(hunters$Metric)
hunters_long <- gather(hunters, key=year, value=value, "1985":"2017")
write_excel_csv(hunters_long, "Intermediate/Hunters_long.csv")


#fishing
fish <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="fishing", range="A1:BP20")
fish$Country <- as.factor(fish$Country)
fish$Region <- as.factor(fish$Region)
fish$Metric <- as.factor(fish$Metric)
fish <- fish[fish$Region!="Gulf of Alaska",]
fish_long <- gather(fish, key=year, value=value, "1950":"2014")
write_excel_csv(fish_long, "Intermediate/Fishing_long.csv")

#reindeer
rein <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="reindeerherding", range="A1:CB17")
rein$Country <- as.factor(rein$Country)
rein$Region <- as.factor(rein$Region)
rein$Metric <- as.factor(rein$Metric)
rein_long <- gather(rein, key=year, value=value, "1941":"2017")
write_excel_csv(rein_long, "Intermediate/Reindeer_long.csv")

#################
## Merge all files
#################

#set directories
#wd = "C:/Users/cru016/Documents/CONNECT/Paper_5_state_of_arctic/Analysis/Intermediate"
wd = "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis/Intermediate"

#Industries
industries <- c("Cruise_tourism", "Domestic_tourism", "International_tourism", "Population", "Shipping_distance", "Hunters", "Fishing", "Oilandgas_wells", "Mining", "Reindeer") # we analyse "Shipping_trafficwork" separately

#load and merge files
alldat <- lapply(industries, function(i) {
  print(paste0("loading ", i))
  
  dat <- read.csv(paste0(wd, "/", i, "_long.csv"), header=TRUE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  dat$value_raw <- dat$value
  dat$subIndustry <- i
  
  if(i %in% c("Cruise_tourism", "Domestic_tourism", "International_tourism")){
    dat$Industry <- "Tourism"
  } else {
    dat$Industry <- i
  }
  
  if(i == "Shipping_distance"){ 
    dat <- dat %>% filter(Country!="Total") %>% 
      group_by(Country, Region, Industry, subIndustry, year) %>% 
      summarise(value=sum(value, na.rm=TRUE)) %>% #sum shipping distance across all types of shipping
      mutate(Metric="nautical miles sailed all shipcats") %>% 
      mutate(value_raw=value)
    dat$value[dat$value==0] <- 1 #add small constant to make growth rate calculation possible
    
  } 
  
  if(i == "Fishing"){ 
    dat <- dat %>% filter(Region!="Total") 
  } 
  
  
  if(i == "Cruise_tourism"){ 
    #Murmansk cruise tourism statistics use a different metric than elsewhere, number of ship calls as opposed to passengers.
    #We convert ship calls to an estimation of passenger numbers by multiplying the number of calls by the average vessel capacity of ships calling in 2016 (437.4 passengers).
    #adjust murmansk
    dat$value[dat$Region=="Murmansk"] <-  dat$value[dat$Region=="Murmansk"]*437.4
    dat[dat$Region=="Murmansk", "Metric"] <- "Passengers"
    #drop other metrics
    dat <- dat %>% filter(Metric=="Passengers")
  } 
  
  if(i == "Hunters") { 
    #drop other types of hunting
    dat <- dat %>% filter(Metric %in% c("All hunting/trapping/fishing licenses", "Number of hunters who paid game management fees", "Registered hunters", "Paid hunting permits", "Sports hunters"))
    #drop extra column
    dat <- dat[, which(!names(dat)=="Region_original")]
  }
  
  if(i == "Oilandgas_wells") {
    #remove duplicated data: drop Region="all" - this is a sum of onshore and offshore; and purpose="all" - this is sum of exploration & development
    dat <- dat %>% filter(Region!="All" & Metric!="All") %>% #have to explicitly select NAs to get Alaska etc
      group_by(Country, Region, Metric, year) %>% #the metric column tells us whether exploration or dev
      mutate(subIndustry=paste("Oilandgas", Metric, sep="_")) #split Industry into exploration and development      
    dat$Metric <- "n_wells"
    dat$Industry <- "OilandGas"
    
    #change value to cumsum
    dat <- dat %>% group_by(Country,Region,subIndustry) %>% 
      arrange(year) %>%
      mutate(value=cumsum(value_raw)) %>% #cumulative sum of n wells
      mutate(value=replace(value, value==0, NA)) #replace zeros with NA
  }
  
  return(data.frame(dat))
  
})
alldat <- bind_rows(alldat)
#alldat$value[alldat$value==0] <- NA #replace zeros with NA, otherwise anngrowthfun returns Inf
alldat$Country[alldat$Country %in% c("Svalbard & Jan Mayen", "Svalbard and Jan Mayen")] <- "Svalbard"
alldat <- alldat %>% mutate_at(c("Country", "Region", "Industry", "subIndustry"), funs(factor)) %>% #convert to factors
  filter(!is.na(value)) #drop NAs
write_excel_csv(alldat, paste0(dirname(wd), "/Output/All_industries_long.csv")) #for fileEncoding = "UTF-8-BOM"



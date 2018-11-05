#This script summarises the mining database compiled as part of CONNECT "MiningDatabaseOct2018_macro.xlsm"

### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)
require(sf)


wd <- "D:/Box Sync/Arctic/Data/Landuse/Mining/Ojas_mining_database_2018"
setwd(wd)

#Raw data
mines <- read_excel("Mining data/MiningDatabaseOct2018_macro.xlsm", sheet="MiningData_merged")
borders <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic/SOA_borders_for_popn2.shp")

#filter data
mines <- mines %>% 
          filter(ArcticAmap==1 & Status!="Historic") %>% #select mines in study region, not historic
          filter(MineralGrp!="Industrial minerals" & MineralGrp!="Industrial rock") %>% #drop industrial minerals (~55 mines)
          filter(Size %in% c(NA, "Large", "Medium", "Medium sized deposit", "Large deposit", "Very large", "Very large deposit", "Unknown", "NA", "Very Large", "Potentially large")) %>% #drop small mines
          select(Name, Country, MineralGrp, Status, Start, Stop, POINT_X, POINT_Y) 

#add region
#first drop all the greenland licence areas which have no spatial cooordinates
mines_sf <- st_as_sf(mines[!is.na(mines$POINT_X),], coords=c("POINT_X", "POINT_Y"), crs=4326) %>%
         st_transform(crs=st_crs(borders))
borders <- borders %>% select(Region)
mines_region <- st_intersection(mines_sf, borders) 
st_geometry(mines_region) <- NULL

#reclassify status
mines_region$Status[mines_region$Status %in% c("Prefeasibility", "Conceptual", "Feasibility","Bankable")] <- "Exploration"
mines_region$Status[mines_region$Status %in% c("Under development", "Construction", "Operating", "Restart", "Active mine")] <- "Exploitation"
mines_region$Status[mines_region$Status %in% c("Closed", "Suspended", "Care and Maintenance",  "Closed mine")] <- "Closed"

#drop rows that are closed, with no closure date
mines_region <- mines_region %>% filter(!(Status=="Closed" &  is.na(Stop)))
write.csv(mines_region, "processed/Mining_listofMines_withdates.csv")

#where mines are closed and have no start date, add the start date of 1900
mines_region$Start[is.na(mines_region$Start) & mines_region$Status=="Closed"] <- 1900
#where mines have no start date, add the start date of 1990
mines_region$Start[is.na(mines_region$Start)] <- 1990
#add stop date of 2017 (i.e. where mines are still operating)
mines_region$Stop[is.na(mines_region$Stop)] <- 2017

#Reformat to long
mineslong <- data.frame()
for (i in 1:nrow(mines_region)){
  years <- seq(mines_region$Start[i], mines_region$Stop[i], 1)
  for(yr in years) {
    currdf <- data.frame(c(mines_region[i, c("Country", "Region", "Status")], yr))
    names(currdf) <- c("Country", "Region", "Status", "year")
    mineslong <- rbind(mineslong, currdf)
}}

#Summarise data
#How many mines are operational in each year
minesOp <- mineslong %>% 
                filter(Status %in% c("Closed", "Exploitation")) %>%
                group_by(Country, Region, year) %>% summarise(nmines=n()) %>%
                filter(year %in% 1900:2017) %>%
                spread(year, nmines)
write_excel_csv(minesOp, "processed/Mining_nmines_operational_byyear.csv")

#How many mines are under exploitation in each year
minesExpl <- mineslong %>% 
                filter(Status=="Exploration") %>%  
                group_by(Country, Region, year) %>% summarise(nmines=n()) %>%
                filter(year %in% 1900:2017) %>%
                spread(year, nmines)
write_excel_csv(minesExpl, "processed/Mining_nmines_exploration_byyear.csv")

###
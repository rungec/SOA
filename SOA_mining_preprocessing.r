#This script summarises the mining database compiled as part of CONNECT "MiningDatabaseOct2018_macro.xlsm"

### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Intermediate"
setwd(wd)

#Raw data
mines <- read_excel("D:/Box Sync/Arctic/Data/Landuse/Mining/Ojas_mining_database_2018/Mining data/MiningDatabaseOct2018_macro.xlsm", sheet="MiningData_merged")

#filter data
mines <- mines %>% 
          filter(ArcticAmap==1 & Status!="Historic") %>% #select mines in study region, not historic
          filter(MineralGrp!="Industrial minerals" & MineralGrp!="Industrial rock") %>% #drop industrial minerals (~55 mines)
          filter(Size %in% c(NA, "Large", "Medium", "Medium sized deposit", "Large deposit", "Very large", "Very large deposit", "Unknown", "NA", "Very Large", "Potentially large")) %>% #drop small mines
          select(Name, Country, Region2, MineralGrp, Status, Start, Stop) 

#Set up data
mines$Status[mines$Status %in% c("Prefeasibility", "Conceptual", "Feasibility","Bankable")] <- "Exploration"
mines$Status[mines$Status %in% c("Under development", "Construction", "Operating", "Restart", "Active mine")] <- "Exploitation"
mines$Status[mines$Status %in% c("Closed", "Suspended", "Care and Maintenance",  "Closed mine")] <- "Closed"


#drop rows that are closed, with no closure date
mines <- mines %>% filter(!(Status=="Closed" &  is.na(Stop)))
write.csv(mines, "Mining_listofMines_withdates.csv")

#where mines are closed and have no start date, add the start date of 1900
mines$Start[is.na(mines$Start) & mines$Status=="Closed"] <- 1900
#where mines have no start date, add the start date of 1990
mines$Start[is.na(mines$Start)] <- 1990
#add stop date of 2017 (i.e. where mines are still operating)
mines$Stop[is.na(mines$Stop)] <- 2017

#Reformat to wide
mineslong <- data.frame()
for (i in 1:nrow(mines)){
  years <- seq(mines$Start[i], mines$Stop[i], 1)
  for(yr in years) {
    currdf <- data.frame(c(mines[i, c("Country", "Region2", "Status")], yr))
    names(currdf) <- c("Country", "Region", "Status", "year")
    mineslong <- rbind(mineslong, currdf)
}}

#Summarise data
#How many mines are operational in each year
minesOp <- mineslong %>% 
                filter(Status %in% c("Closed", "Exploitation")) %>%
                group_by(Country, Region, year) %>% summarise(nmines=n()) %>%
                filter(year %in% 1900:2017) 
               # spread(year, nmines)
write_excel_csv(minesOp, "Mining_nmines_operational_byyear.csv")

#How many mines are under exploitation in each year
minesExpl <- mineslong %>% 
                filter(Status=="Exploration") %>%  
                group_by(Country, Region, year) %>% summarise(nmines=n()) %>%
                filter(year %in% 1900:2017) 
                #spread(year, nmines)
write_excel_csv(minesExpl, "Mining_nmines_exploration_byyear.csv")

###
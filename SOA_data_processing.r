
### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis"
setwd(wd)

#Ship dist
shipdist <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_distancesailed", range="A1:J92")
shipdist$Country <- as.factor(shipdist$Country)
shipdist$Region <- as.factor(shipdist$region)
shipdist$Metric <- as.factor(shipdist$ShipCat)
shipdist_long <- gather(shipdist, key=year, value=value, "X2012":"X2018")
write_excel_csv(shipdist_long, "Intermediate/Shipping_distance_long.csv")

#Ship trafficwork
shipdist <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_trafficwork", range="A1:H3676")
shipdist$Country <- as.factor(shipdist$Country)
shipdist$Region <- as.factor(shipdist$region)
shipdist$Metric <- as.factor(shipdist$ShipCat)
shipdist$ShipWt <- as.factor(shipdist$shipweight)
write_excel_csv(shipdist, "Intermediate/Shipping_trafficwork_long.csv")

#Ship vol
shipvol <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_voltransported", range="A1:AB26")
shipvol$Country <- as.factor(shipvol$Country)
shipvol$Metric <- as.factor(shipvol$Cargo)
shipvol$Region <- as.factor(shipvol$Region)
shipvol_long <- gather(shipvol, key=year, value=value, "X1991":"X2016")
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
wells <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="oilandgas_wells", range="A1:CJ49")
wells$Country <- as.factor(wells$Country)
wells$Region <- as.factor(wells$Region)
wells$Metric <- as.factor(wells$Type)
wells_long <- gather(wells, key=year, value=value, "1920":"2018")
write_excel_csv(wells_long, "Intermediate/Oilandgas_wells_long.csv")

#mining
mines <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="mining", range="A1:DQ46")
mines$Country <- as.factor(mines$Country)
mines$Region <- as.factor(mines$Region)
mines$Metric <- as.factor(mines$Metric)
mines_long <- gather(mines, key=year, value=value, "1900":"2017")
write_excel_csv(mines_long, "Intermediate/Mining_long.csv")

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
fish_long <- gather(fish, key=year, value=value, "1950":"2014")
write_excel_csv(fish_long, "Intermediate/Fishing_long.csv")

#reindeer
rein <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="reindeerherding", range="A1:CB17")
rein$Country <- as.factor(rein$Country)
rein$Region <- as.factor(rein$Region)
rein$Metric <- as.factor(rein$Metric)
rein_long <- gather(rein, key=year, value=value, "1941":"2017")
write_excel_csv(rein_long, "Intermediate/Reindeer_long.csv")

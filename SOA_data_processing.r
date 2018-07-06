
### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(readxl)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic"
setwd(wd)

#Ship dist
shipdist <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_distancesailed")
shipdist$Country <- as.factor(shipdist$Country)
shipdist$Region <- as.factor(shipdist$region)
shipdist$Metric <- as.factor(shipdist$ShipCat)
shipdist_long <- gather(shipdist, key=year, value=value, "2012":"2018")
write_excel_csv(shipdist_long, "Tables/Shipping_distance_long.csv")

#Ship vol
shipvol <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="shipping_voltransported", range="A1:AB26")
shipvol$Country <- as.factor(shipvol$Country)
shipvol$Metric <- as.factor(shipvol$Cargo)
shipvol$Region <- as.factor(shipvol$Region)
shipvol_long <- gather(shipvol, key=year, value=value, "1991":"2017")
write_excel_csv(shipvol_long, "Tables/Shipping_volume_long.csv")

#population
popn <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="population", range="A1:AF54")
popn$Country <- as.factor(popn$Country)
popn$Region <- as.factor(popn$Region)
popn$Metric <- as.factor(popn$Metric)
popn_long <- gather(popn, key=year, value=value, "1990":"2018")
write_excel_csv(popn_long, "Tables/Population_long.csv")

#tourism
tur_cruise <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_cruise", range="A1:AF18")
tur_cruise$Country <- as.factor(tur_cruise$Country)
tur_cruise$Region <- as.factor(tur_cruise$Region)
tur_cruise$Metric <- as.factor(tur_cruise$Metric)
tur_cruise_long <- gather(tur_cruise, key=year, value=value, "1990":"2018")
write_excel_csv(tur_cruise_long, "Tables/Cruise_tourism_long.csv")

tur_domestic <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_domestic", range="A1:U25")
tur_domestic$Country <- as.factor(tur_domestic$Country)
tur_domestic$Region <- as.factor(tur_domestic$Region)
tur_domestic$Metric <- as.factor(tur_domestic$Metric)
tur_domestic_long <- gather(tur_domestic, key=year, value=value, "2000":"2017")
write_excel_csv(tur_domestic_long, "Tables/Domestic_tourism_long.csv")

tur_int <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="tourism_international", range="A1:U25")
tur_int$Country <- as.factor(tur_int$Country)
tur_int$Region <- as.factor(tur_int$Region)
tur_int$Metric <- as.factor(tur_int$Metric)
tur_int_long <- gather(tur_int, key=year, value=value, "2000":"2017")
write_excel_csv(tur_int_long, "Tables/International_tourism_long.csv")

tur_all <- rbind(tur_cruise_long, tur_domestic_long, tur_int_long)
tur_all$Touristtype <- rep(c("Cruise", "Domestic", "International"), times=c(nrow(tur_cruise_long), nrow(tur_domestic_long), nrow(tur_int_long)))
write_excel_csv(tur_all, "Tables/All_tourism_long.csv")


#Oil and gas
#wells
wells <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="oilandgas_wells", range="A1:CJ46")
wells$Country <- as.factor(wells$Country)
wells$Region <- as.factor(wells$Region)
wells$Metric <- as.factor(wells$Type)
wells_long <- gather(wells, key=year, value=value, "1920":"2018")
write_excel_csv(wells_long, "Tables/Oilandgas_wells_long.csv")


#Hunting
#number of hunters
hunters <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="Hunters", range="A1:AK47")
hunters$Country <- as.factor(hunters$Country)
hunters$Region <- as.factor(hunters$Region)
hunters$Metric <- as.factor(hunters$Metric)
hunters_long <- gather(hunters, key=year, value=value, "1985":"2017")
write_excel_csv(hunters_long, "Tables/Hunters_long.csv")


#fishing
fish <- read_excel("Input/State_of_Arctic_all_data.xlsx", sheet="fishing", range="A1:BP20")
fish$Country <- as.factor(fish$Country)
fish$Region <- as.factor(fish$Region)
fish$Metric <- as.factor(fish$Metric)
fish_long <- gather(fish, key=year, value=value, "1950":"2014")
write_excel_csv(fish_long, "Tables/Fishing_long.csv")


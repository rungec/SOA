#Intensity

require(sf)

area_tourism <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic/SOA_borders_for_tourism2.shp") 
  st_geometry(area_tourism) <- NULL
area_popn <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic/SOA_borders_for_popn2.shp")
  st_geometry(area_popn) <- NULL
area_eez <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic/EEZ_plus_highseas_noland.shp")
  st_geometry(area_eez) <- NULL
area_fishing <- read_sf("D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic/SOA_borders_for_fishing.shp")
  st_geometry(area_fishing) <- NULL

intensitydf <- read.csv("D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis/Model/All_industries_intensity.csv", header=TRUE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) #for fileEncoding = "UTF-8-BOM"

industries <- unique(intensitydf$subIndustry)

dput(industries)
c("Hunters", "Population", "Cruise_tourism", "Domestic_tourism", 
  "International_tourism", "Shipping_distance", "Mining", "Reindeer", 
  "Fishing")




left_join(country_lookup, by="region") %>% 


#cruise tourism by coastline
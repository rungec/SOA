# This script combines and summarises shipping traffic data for the Arctic region, 2012 - 2018 
#2012-2018 data from https://havbase.no/havbase_arktis Accessed 08/05/2018
#older data from Ocean Economics Accessed 08/05/2018

require(tidyverse)
require(lubridate)
require(readxl)
require(ggplot2)
require(scales) #for scale_x_datetime




#########################
### Preliminary processing havbase data ----

wd <- "D:/Box Sync/Arctic/Data/Transport and accessibility/Shipping/Havbase_arktis"
setwd(wd)

filelist <- list.files(paste0(wd, "/raw"), pattern="DistanceSailedExport*", full.names=TRUE)
shipdatL <- lapply(filelist, function (i){
  currsheet <- read_excel(i, skip = 2, col_types = "text")
  currsheet[, 3:10] <- do.call(cbind, lapply(currsheet[, 3:10], function(x) {
                                        as.numeric(gsub('\\s+', '',x))
                                        })
                               )
  currsheet$region <- strsplit(strsplit(basename(i), ".xlsx")[[1]], "_")[[1]][2]
  return(currsheet)
})
shipdatdf <- do.call(rbind, shipdatL)

#fix the date column
shipdatdf$Date <- as.POSIXct(paste(shipdatdf$Periode,"-01",sep=""))
shipdatdf$Year <- year(shipdatdf$Date)
shipdatdf$Month <- month(shipdatdf$Date)

#categorise the ship types
shipdatdf$ShipCat <- c("Other")
shipdatdf$ShipCat[shipdatdf$Skipstype %in% c("Oljetankere", "Kjemikalie-/produkttankere", "Gasstankere") ] <- c("Oil or gas tanker")
shipdatdf$ShipCat[shipdatdf$Skipstype %in% c("Bulkskip", "Stykkgodsskip", "Konteinerskip", "Ro Ro last", "Kjøle-/fryseskip") ] <- c("Goods transport")
shipdatdf$ShipCat[shipdatdf$Skipstype %in% c("Fiskefartøy") ] <- c("Fishing")
shipdatdf$ShipCat[shipdatdf$Skipstype %in% c("Passasjer") ] <- c("Passenger")

write.csv(shipdatdf, "intermediate/DistancesSailed_alldata.csv", row.names=FALSE) 

#Summarise and export
t0 <- shipdatdf %>% group_by(region, ShipCat, Year) %>% 
      summarise(dist_sailed=sum(`Total distanse`)) %>% 
      spread(Year, dist_sailed) %>% data.frame()
write.csv(t0, "intermediate/DistancesSailed_byregion_shipcat_year.csv", row.names=FALSE)

#Summarise and export
t02 <- shipdatdf %>% group_by(region, Year) %>% 
      summarise(dist_sailed=sum(`Total distanse`)) %>% 
      spread(Year, dist_sailed) %>% data.frame()
write.csv(t02, "intermediate/DistancesSailed_byregion_year.csv", row.names=FALSE)

#Summarise and export shiptraffic
weight_lookup <- data.frame(shipweight=c("<1000 GT", ">=100000 GT", "1000-4999 GT", "10000-24999 GT", "25000-49999 GT", "5000-9999 GT", "50000-99999 GT"), 
                            shipweight_mid=c(1000, 100000, 3000, 17500, 37500, 7500, 75000), stringsAsFactors=FALSE)
country_lookup <- data.frame(region=c("AlaskanEEZ", "CanadianEEZ", "FaroesEEZ", "GreenlandEEZ", "IcelandEEZ", "JanMayenEEZ", "NorwayEEZ", "OpenArctic", "OpenBarentsSea", "OpenDavisStrait", "OpenNorthAtlantic", "OpenNorwegianSea", "RussianEEZ", "SvalbardFisheriesZone", "UKEEZ"),
                             Country=c("USA", "Canada", "Faroe Islands", "Greenland", "Iceland", "Svalbard & Jan Mayen", "Norway", "High Seas", "High Seas", "High Seas", "High Seas", "High Seas", "Russia", "Svalbard & Jan Mayen", "UK"))

shipweightdf <- shipdatdf %>% gather(shipweight, dist_sailed, "<1000 GT":">=100000 GT") 
tsw <- shipweightdf %>% group_by(region, ShipCat, shipweight, Year) %>% 
                summarise(dist_sailed=sum(dist_sailed)) %>% 
                filter(region!="Total")
#tsw <- shipweightdf %>% filter(ShipCat=="Goods transport") %>% group_by(shipweight, Year) %>% summarise(dist_sailed=sum(dist_sailed))
tsw <- tsw %>% left_join(weight_lookup, by="shipweight") %>% #add shipweight as number (midpoint)
               left_join(country_lookup, by="region") %>% #add country
               mutate(traffic_work_gTnm = shipweight_mid*dist_sailed) #calculate traffic work

write.csv(tsw, "ArcticShipping_byregion_vesselweight_year.csv", row.names=FALSE)


#########################
#Preliminary processing OceanEconomics

wd <- "D:/Box Sync/Arctic/Data/Transport and accessibility/Shipping/OceanEconomics"
setwd(wd)

filelist <- list.files(wd, pattern="ArcticShippingData*")
oedatL <- lapply(filelist, function (i) read.csv(i, stringsAsFactors = FALSE, header=TRUE))
oedatdf <- do.call(rbind, oedatL)
#drop rows with nas
oedatdf <- oedatdf[!is.na(oedatdf$Year), ]
#fix typo
oedatdf$Cargo[oedatdf$Cargo=="minerl"] <- "minerals" 
#drop comma from numbers
names(oedatdf)[names(oedatdf)=="Weight..Tons."] <- "Weight_tons"
oedatdf$Weight_tons <- as.numeric(gsub(",", "", oedatdf$Weight_tons))

write.csv(oedatdf, "ArcticShipping_alldata.csv", row.names=FALSE) 

#Summarise and export
#drop red dog mine - data is duplicated
oedatdf <- oedatdf[oedatdf$Ports!="Red Dog Mine Port", ]
oe0 <- oedatdf %>% group_by(Region, Cargo, Year) %>% 
  summarise(Vol_transported_tons=sum(`Weight_tons`)) %>% 
  spread(Year, Vol_transported_tons) %>% data.frame()
write.csv(oe0, "ArcticShipping_byregion_Cargotype_year.csv", row.names=FALSE)



#########################
#plot havebase

#By Region across time
t1 <- shipdatdf %>% group_by(region, Date) %>% summarise(dist_sailed=sum(`Total distanse`))

ggplot(t1, aes(x=Date, y=dist_sailed, col=region)) +
  geom_line()

ggplot(t1, aes(x=Date, y=dist_sailed, fill=region)) +
  geom_bar(stat="identity") 

#By ship type
t2 <- shipdatdf %>% group_by(region, ShipCat) %>% summarise(dist_sailed=sum(`Total distanse`))

ggplot(t2, aes(fill=ShipCat, y=dist_sailed, x=region)) +
  geom_bar(stat="identity") +
  coord_flip() 

ggplot(t2, aes(x=ShipCat, y=dist_sailed, fill=region)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(facets = "region")

#By ship type and date
t3 <- shipdatdf %>% group_by(ShipCat, Date) %>% summarise(dist_sailed=sum(`Total distanse`))
ggplot(t3, aes(x=Date, y=dist_sailed, col=ShipCat)) +
  geom_line() +
  facet_wrap(facets = "ShipCat")

#by region and shiptype
t4 <- shipdatdf %>% group_by(region, ShipCat, Year) %>% summarise(dist_sailed=sum(`Total distanse`))

ggplot(t4, aes(x=Year, y=dist_sailed, col=ShipCat)) +
  geom_line() +
  xlim(2012, 2017) +
  facet_wrap(facets="region", scale="free_y")

#by ship weight
ggplot(tsw, aes(x=Year, y=dist_sailed, col=shipweight)) +
  geom_line() +
  xlim(2012, 2017) +
  facet_wrap(facets="region", scale="free_y")

#########################
#plot OceanEconomics

#By Region across time
oe1 <- oedatdf %>% group_by(Region, Cargo, Year) %>% summarise(vol_transported=sum(Weight_tons))

ggplot(oe1, aes(x=Year, y=vol_transported, col=Region)) +
  geom_line()

oe2 <- oedatdf %>% group_by(Region, Cargo, Year) %>% summarise(vol_transported=sum(Weight_tons))
ggplot(oe2, aes(x=Year, y=vol_transported, fill=Cargo)) +
  geom_bar(stat="identity") +
  facet_wrap(facets = "Region")

oe2 <- oedatdf %>% group_by(Region, Cargo, Year) %>% summarise(vol_transported=sum(Weight_tons))
ggplot(oe2, aes(x=Year, y=vol_transported, col=Cargo)) +
  geom_line() +
  xlim(2003, 2017)+
  facet_wrap(facets = "Region", scales="free_y")

ggplot(oe2, aes(x=Year, y=vol_transported, fill=Region)) +
  geom_bar(stat="identity") +
  facet_wrap(facets = "Cargo")


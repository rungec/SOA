### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(sf)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic"
datawd <- "D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic"
setwd(wd)


#country borders
borders <- read_sf(paste0(dsn=dirname(datawd), "/60degreesN"), layer="CountryBorders_45degreesN_lambert_noAMAP_1000kmenvelope")


#TOURISM
#Cruise tourism
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism2")
#read in table
tbl <- read.csv(paste0(wd, "/Tables/Cruise_tourism_summarystats.csv"))
#manipulate table
tbl_ed <- tbl[tbl$Metric=="Passengers", ]
murmansk <- tbl[tbl$Region=="Murmansk", ]
murmansk$Intensity <- murmansk$Intensity*437
tbl_ed <- rbind(tbl_ed, murmansk)
#merge table to sf
cruisetourism <- merge(template, tbl_ed, by=c("Country", "Region"), all.x=TRUE)

#we calculate the intensity for murmansk using the average passenger capacity of the vessels docking in Murmansk
#437 passengers
#plot
ggplot(cruisetourism) +
  geom_sf(data=borders, fill="grey90", color="grey95") + #add borders
  geom_sf(data=cruisetourism, aes(fill=Intensity), color=NA, show.legend=TRUE) +
  scale_fill_viridis_c(option="magma", direction=-1, 
                       name="Annual passengers", labels = scales::comma,
                       na.value="grey80") +
    #labs(x = NULL, y = NULL, 
        # title = "Arctic cruise tourism",
         #subtitle = "",
         #caption = "")
  coord_sf(crs = st_crs(cruisetourism), datum = NA) + #drop graticules
  theme(panel.grid=element_blank())

c("AnnGrowth_02_07","AnnGrowth_07_12", "AnnGrowth_12_17", "Intensity")

#to plot the intensity at the centroids edit this
cruisetourism$mid <- sf::st_centroid(cruisetourism$geometry)
cruisetourism_int <- cruisetourism[!is.na(cruisetourism$Intensity), ]

ggplot(cruisetourism) +
  geom_sf(colour = "white") +
  geom_sf(data=cruisetourism_int, aes(geometry = mid, size = Intensity), show.legend = "point") +
  scale_size_area(labels = scales::comma, breaks=c(1000, 5000, 100000, 1000000))


https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#a-very-basic-map
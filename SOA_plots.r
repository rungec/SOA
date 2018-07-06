### Set up libraries ----
require(tidyverse)
require(ggplot2)
require(sf)
require(gtable)


wd <- "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic"
datawd <- "D:/Box Sync/Arctic/Data/Boundaries/Arctic_circle/State_of_Arctic"
setwd(wd)

#############
#PRELIMINARY ----
#############
#Set up shapefiles ----
#country borders
borders <- read_sf(dsn=paste0(dirname(datawd), "/60degreesN"), layer="CountryBorders_45degreesN_lambert_noAMAP_1000kmenvelope")
#soa county borders
soaborders <- read_sf(dsn=paste0(datawd, "/Parts"), layer="SOA_countyborders_smoothed")
#AMAP borders
amap <- read_sf(dsn=paste0(dirname(datawd), "/AMAP"), layer="AMAP_updatedRussia_clipto60N_SOA")

#metrics <- c("Cruise_tourism", "Domestic_tourism", "International_tourism", "Population", "Shipping_distance", "Shipping_volume", "Hunters", "Fishing", "Oilandgas_wells")


#############
#Set up functions ----
#function to add the min value on the legend and change length of bars
#draws on code from 
#https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#a-very-basic-map
extendLegendWithExtremes <- function(p, currcol, ndecimals){
  p_grob <- ggplotGrob(p)
  legend <- gtable_filter(p_grob, "guide-box")
  legend_grobs <- legend$grobs[[1]]$grobs[[1]]
  #  # grab the first key of legend
  #  legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
  #  legend_first_key$widths <- unit(2, units = "cm")
  #  # modify its width and x properties to make it longer
  #  legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
  #  legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")
  #  
  #  # last key of legend
  #  legend_last_key <- gtable_filter(legend_grobs, "key-3-5-1")
  # legend_last_key$widths <- unit(2, units = "cm")
  #  # analogous
  #  legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
  #  legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")
  #  
  #  # grab the last label so we can copy it and also shift its position
  nlabels <-  dim(gtable_filter(legend_grobs, "label"))[[2]]
  #last_text <- paste("label", (nlabels-1), nlabels, sep="-")
  legend_last_label <- gtable_filter(legend_grobs, "label")[1, nlabels]
  #legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")
  #  
  #  # Insert new color legend back into the combined legend
  #  legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <- 
  #    legend_first_key$grobs[[1]]
  #  legend_grobs$grobs[legend_grobs$layout$name == "key-3-5-1"][[1]] <- 
  #    legend_last_key$grobs[[1]]
  #  legend_grobs$grobs[legend_grobs$layout$name == "label-5-5"][[1]] <- 
  #    legend_last_label$grobs[[1]]
  
  # finally, I need to create a new label for the minimum value 
  new_first_label <- legend_last_label$grobs[[1]]
  new_first_label$children[[1]]$label <- round(min(currcol, na.rm = T), ndecimals)*100
  newname <- paste("label", strsplit(legend_last_label$layout$name, "-")[[1]][2], "0", sep="-")
  new_first_label$children[[1]]$x <- unit(2, units = "pt") #this shifts the position of the label
  new_first_label$children[[1]]$hjust <- 1
  
  legend_grobs <- gtable_add_grob(legend_grobs, 
                                  new_first_label, 
                                  t = 6, 
                                  l = 2, 
                                  name = newname, 
                                  clip = "off")
  legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
  p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend
  
  # the plot is now drawn using this grid function
  grid::grid.newpage()
  grid::grid.draw(p_grob)
}

plotfunGrowth <- function(map_data, mapped_col, pretty_breaks, mycols, ndecimals, legendtitle, titletext, subtext, ...) {
  #set up legend breaks
  pretty_breaks <- pretty_breaks/100
  currcol <- map_data[, which(names(map_data)==mapped_col)] %>% st_set_geometry(NULL) %>% unlist()
  minVal <- min(currcol, na.rm=TRUE)
  maxVal <- max(currcol, na.rm=TRUE)
  labels <- c()
  brks <- c(minVal, pretty_breaks, maxVal)
  for(i in 1:length(brks)){
    labels <- c(labels,round(brks[i + 1], ndecimals)*100)
  }
  labels <- labels[1:length(labels)-1]
  map_data$brks <- cut(currcol, 
                       breaks = brks, 
                       include.lowest = TRUE, 
                       labels = labels)
  brks_scale <- levels(map_data$brks)
  labels_scale <- rev(brks_scale)
  
  #set up plot
  p <- ggplot(map_data) +
        geom_sf(data=borders, fill="grey90", color="grey95") + #add borders
        geom_sf(data=amap, fill="white", color=NA) + #add amap
        #geom_sf(data=soaborders, fill=NA, color="grey85") + #add countyborders
        geom_sf(data=map_data, aes(fill=brks), color=NA, show.legend=TRUE) +
        labs(x = NULL, y = NULL, 
        title = titletext,
        subtitle = subtext, ...) +
        coord_sf(crs = st_crs(map_data), datum = NA, expand=FALSE) + #drop graticules
        theme(panel.grid=element_blank(), legend.position="bottom", 
              panel.background = element_rect(fill="grey90"), 
              plot.background = element_rect(fill = "grey90", color="grey90"),
              legend.background = element_rect(fill="grey90"),
              plot.margin = unit(c(2,2,2,2), "mm")) +
        scale_fill_manual(values = mycols, breaks = rev(brks_scale),
                          name = legendtitle,
                          drop = FALSE, labels = labels_scale,
                          guide = guide_legend(
                            direction = "horizontal",
                            keyheight = unit(2, units = "mm"),
                            keywidth = unit(70 / length(labels), units = "mm"),
                            title.position = 'top',
                            # I shift the labels around, the should be placed 
                            # exactly at the right end of each legend key
                            title.hjust = 0.5, label.hjust = 1, nrow = 1,
                            byrow = T, # also the guide needs to be reversed
                            reverse = T, label.position = "bottom"))
  
  outname <- paste(titletext, subtext, sep="_")
  outname <- paste0("Figures/", gsub(" ", "_", outname), ".png")
  png(outname, height=6, width=4.41, units="in", res=300)
  extendLegendWithExtremes(p=p, currcol=currcol, ndecimals=ndecimals)
  dev.off()
}

plotfunIntensity <- function(map_data, mapped_col, pretty_breaks, mycols, ndecimals, legendtitle, titletext, subtext, ...) {
  #set up legend breaks
  currcol <- map_data[, which(names(map_data)==mapped_col)] %>% st_set_geometry(NULL) %>% unlist()
  minVal <- min(currcol, na.rm=TRUE)
  maxVal <- max(currcol, na.rm=TRUE)
  labels <- c()
  brks <- c(minVal, pretty_breaks, maxVal)
  for(i in 1:length(brks)){
    labels <- c(labels,round(brks[i + 1], ndecimals))
  }
  labels <- labels[1:length(labels)-1]
  map_data$brks <- cut(currcol, 
                       breaks = brks, 
                       include.lowest = TRUE, 
                       labels = labels)
  brks_scale <- levels(map_data$brks)
  labels_scale <- rev(brks_scale)
  
  #set up plot
  p <- ggplot(map_data) +
        geom_sf(data=borders, fill="grey90", color="grey95") + #add borders
        geom_sf(data=amap, fill="white", color=NA) + #add amap
        #geom_sf(data=soaborders, fill=NA, color="grey85") + #add countyborders
        geom_sf(data=map_data, aes(fill=brks), color=NA, show.legend=TRUE) +
        labs(x = NULL, y = NULL, 
             title = titletext,
             subtitle = subtext, ...)+
        coord_sf(crs = st_crs(map_data), datum = NA, expand=FALSE) + #drop graticules
        theme(panel.grid=element_blank(), legend.position="bottom", 
              panel.background = element_rect(fill="grey90"), 
              plot.background = element_rect(fill = "grey90", color="grey90"),
              legend.background = element_rect(fill="grey90"),
              plot.margin = unit(c(2,2,2,2), "mm")) +
        scale_fill_manual(values = mycols, breaks = rev(brks_scale),
                          name = legendtitle,
                          drop = FALSE, labels = labels_scale,
                          guide = guide_legend(
                            direction = "horizontal",
                            keyheight = unit(2, units = "mm"),
                            keywidth = unit(70 / length(labels), units = "mm"),
                            title.position = 'top',
                            # I shift the labels around, the should be placed 
                            # exactly at the right end of each legend key
                            title.hjust = 0.5, label.hjust = 1, nrow = 1,
                            byrow = T, # also the guide needs to be reversed
                            reverse = T, label.position = "bottom"))
  #save plot
  outname <- paste(titletext, subtext, sep="_")
  outname <- paste0("Figures/", gsub(" ", "_", outname), ".png")
  png(outname, height=6, width=4.41, units="in", res=300)
  print(p)
  dev.off()
}

###########################
#MAIN PROCESSING ----
###########################

#####################
#CRUISE TOURISM ----
#read in template
  template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism2")
#read in table
  tbl <- read.csv(paste0(wd, "/Tables/Cruise_tourism_summarystats.csv"))
#manipulate table
  #extract only passenger numbers
  tbl_ed <- tbl[tbl$Metric=="Passengers", ] 
  #we calculate the intensity for murmansk using the average passenger capacity of the vessels docking in Murmansk
  #437 passengers
  murmansk <- tbl[tbl$Region=="Murmansk", ]
  murmansk$Intensity <- murmansk$Intensity*437
  tbl_ed <- rbind(tbl_ed, murmansk)
  #merge table to sf
  cruise_data <- merge(template, tbl_ed, by=c("Country", "Region"), all.x=TRUE)
  cruise_data$Intensity_thou <- cruise_data$Intensity/1000

#plot
plotfunIntensity(map_data=cruise_data, 
          mapped_col="Intensity_thou", 
          mycols = rev(viridisLite::magma(8)[3:7]),
          pretty_breaks=c(1, 5, 10, 100), ndecimals=0, 
          legendtitle="Annual passengers, thousands", 
          titletext="Arctic cruise tourism", subtext="Intensity")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_start_end", 
          mycols = c(rev(viridisLite::viridis(7)[1:6])),
          pretty_breaks=c(5, 10, 15, 35), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="All years")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_02_07", 
          mycols = c("grey70", rev(viridisLite::viridis(7)[2:6])),
          pretty_breaks=c(1, 5, 10, 15), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2002-2007")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_07_12", 
          mycols = c("grey50", "grey70", rev(viridisLite::viridis(7)[2:6])),
          pretty_breaks=c(-1, 1, 5, 10, 15), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2007-2012")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_12_17", 
          mycols = c("grey50", "grey70", rev(viridisLite::viridis(7)[1:6])),
          pretty_breaks=c(-1, 1, 5, 10, 15, 35), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2012-2017")

#####################
#DOMESTIC TOURISM ----
  #read in template
  template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism2")
  #read in table
  tbl <- read.csv(paste0(wd, "/Tables/Domestic_tourism_summarystats.csv"))
  #manipulate table
  #NA
  #merge table to sf
  domestic_data <- merge(template, tbl, by=c("Country", "Region"), all.x=TRUE)
  domestic_data$Intensity_thou <- domestic_data$Intensity/1000
  
  #plot
  plotfunIntensity(map_data=domestic_data, 
                   mapped_col="Intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(20, 100, 200, 500, 1000), ndecimals=0, 
                   legendtitle="Domestic tourists, thousands", 
                   titletext="Arctic domestic tourism", subtext="Intensity", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  plotfunGrowth(map_data=domestic_data, 
                mapped_col="AnnGrowth_start_end", 
                mycols = c("grey30", "grey50", "grey70", rev(viridisLite::viridis(7)[4:6])),
                pretty_breaks=c(-5, -1, 1, 5, 10), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic domestic tourism", subtext="All years")
  plotfunGrowth(map_data=domestic_data, 
                mapped_col="AnnGrowth_02_07", 
                mycols = c("grey30", "grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-1, 1, 5, 10, 15), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic domestic tourism", subtext="2002-2007")
  plotfunGrowth(map_data=domestic_data, 
                mapped_col="AnnGrowth_07_12", 
                mycols = c("grey30","grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-5, -1, 1, 5, 10, 15), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic domestic tourism", subtext="2007-2012")
  plotfunGrowth(map_data=domestic_data, 
                mapped_col="AnnGrowth_12_17", 
                mycols = c("grey30", "grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-5, -1, 1, 5, 10, 15), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic domestic tourism", subtext="2012-2017")
  
  
#####################  
#INTERNATIONAL TOURISM ----
  #read in template
  template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism2")
  #read in table
  tbl <- read.csv(paste0(wd, "/Tables/International_tourism_summarystats.csv"))
  #manipulate table
  #NA
  #merge table to sf
  international_data <- merge(template, tbl, by=c("Country", "Region"), all.x=TRUE)
  international_data$Intensity_thou <- international_data$Intensity/1000
  
  #plot
  plotfunIntensity(map_data=international_data, 
                   mapped_col="Intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(10, 50, 150, 500, 1000), ndecimals=0, 
                   legendtitle="International tourists, thousands", 
                   titletext="Arctic international tourism", subtext="Intensity", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  plotfunGrowth(map_data=international_data, 
                mapped_col="AnnGrowth_start_end", 
                mycols = c("grey30", "grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-3, -1, 1, 3, 5, 8), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic international tourism", subtext="All years")
  plotfunGrowth(map_data=international_data, 
                mapped_col="AnnGrowth_02_07", 
                mycols = c("grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(1, 3, 5, 8), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic international tourism", subtext="2002-2007")
  plotfunGrowth(map_data=international_data, 
                mapped_col="AnnGrowth_07_12", 
                mycols = c("grey30","grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-5, -1, 1, 5, 10, 15), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic international tourism", subtext="2007-2012")
  plotfunGrowth(map_data=international_data, 
                mapped_col="AnnGrowth_12_17", 
                mycols = c("grey30", "grey50", "grey70", rev(viridisLite::viridis(7)[3:6])),
                pretty_breaks=c(-5, -1, 1, 5, 10, 15), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic international tourism", subtext="2012-2017")
  

#####################
#HUNTING - HUNTERS ----
  #read in template
  template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism2")
  #read in table
  tbl <- read.csv(paste0(wd, "/Tables/Hunters_summarystats.csv"))
  #manipulate table
  tbl_ed <- tbl[tbl$Metric %in% c("All hunting/trapping/fishing licenses", "Number of hunters who paid game management fees", 
                                  "Registered hunters", "Paid hunting permits", "Sports hunters"), ] 

  #merge table to sf
  hunters_data <- merge(template, tbl_ed, by=c("Country", "Region"), all.x=TRUE)
  hunters_data$Intensity_thou <- hunters_data$Intensity/1000
  
  #plot
  plotfunIntensity(map_data=hunters_data, 
                   mapped_col="Intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(1, 5, 10, 25, 50), ndecimals=0, 
                   legendtitle="Hunters, thousands", 
                   titletext="Arctic hunting - hunters", subtext="Intensity", caption="Alaska = All hunting/trapping/fishing licenses, \nGreenland = Sports hunters, Norway = Registered hunters, \nFinland & Sweden = Paid hunting permits, \nNo data for Russia, Faroes, Iceland, Canada.")
  
  plotfunGrowth(map_data=hunters_data, 
                mapped_col="AnnGrowth_start_end", 
                mycols = c("grey50", "grey70", rev(viridisLite::viridis(5)[2:4])),
                pretty_breaks=c(-1, 1, 3, 5), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic hunting - hunters", subtext="All years")
  plotfunGrowth(map_data=hunters_data, 
                mapped_col="AnnGrowth_02_07", 
                mycols = c("grey50", "grey70", rev(viridisLite::viridis(5)[2:4])),
                pretty_breaks=c(-1, 1, 3, 5), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic hunting - hunters", subtext="2002-2007")
  plotfunGrowth(map_data=hunters_data, 
                mapped_col="AnnGrowth_07_12", 
                mycols = c("grey50", "grey70", rev(viridisLite::viridis(5)[2:4])),
                pretty_breaks=c(-1, 1, 3, 5), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic hunting - hunters", subtext="2007-2012")
  plotfunGrowth(map_data=hunters_data, 
                mapped_col="AnnGrowth_12_17", 
                mycols = c("grey50", "grey70", rev(viridisLite::viridis(5)[2:4])),
                pretty_breaks=c(-1, 1, 3, 5), ndecimals=3, 
                legendtitle="Annual growth, percent", 
                titletext="Arctic hunting - hunters", subtext="2012-2017")  
    

###stop here


  
  
  
  
  
  
  
  
  
  
  
  
  




#to plot the intensity at the centroids edit this
cruisetourism$mid <- sf::st_centroid(cruisetourism$geometry)
cruisetourism_int <- cruisetourism[!is.na(cruisetourism$Intensity), ]

ggplot(cruisetourism) +
  geom_sf(colour = "white") +
  geom_sf(data=cruisetourism_int, aes(geometry = mid, size = Intensity), show.legend = "point") +
  scale_size_area(labels = scales::comma, breaks=c(1000, 5000, 100000, 1000000))

#plot
ggplot(cruisetourism) +
  geom_sf(data=borders, fill="white", color="grey90") + #add borders
  geom_sf(data=amap, fill="grey95", color=NA) + #add amap
  geom_sf(data=cruisetourism, aes(fill=AnnGrowth_12_17), color=NA, show.legend=TRUE) +
  #scale_fill_viridis_c(option="magma", direction=-1, 
  name="Annual passengers, thousand", labels = scales::comma,
na.value="grey80") +
  #labs(x = NULL, y = NULL, 
  # title = "Arctic cruise tourism",
  #subtitle = "",
  #caption = "")
  coord_sf(crs = st_crs(cruisetourism), datum = NA) + #drop graticules
  theme(panel.grid=element_blank()) 
 

scale_fill_distiller(palette = "RdBu"
                     , limits = c(-1,1)*max(abs(grid$z))
)








#####################
#POPULATION ----
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_popn2")
#read in table
tbl <- read.csv(paste0(wd, "/Tables/Population_summarystats.csv"))
#manipulate table
#NA
#merge table to sf
popn_data <- merge(template, tbl, by=c("Country", "Region"), all.x=TRUE)
popn_data$Intensity_thou <- popn_data$Intensity/1000

#plot
plotfunIntensity(map_data=popn_data, 
                 mapped_col="Intensity_thou", 
                 mycols = rev(viridisLite::magma(8)[2:7]),
                 pretty_breaks=c(5, 10, 50, 100, 500), ndecimals=0, 
                 legendtitle="Population, thousands", 
                 titletext="Arctic population", subtext="Intensity")
plotfunGrowth(map_data=popn_data, 
              mapped_col="AnnGrowth_start_end", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1, 3), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic population", subtext="All years")
plotfunGrowth(map_data=popn_data, 
              mapped_col="AnnGrowth_02_07", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1, 3), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic population", subtext="2002-2007")
plotfunGrowth(map_data=popn_data, 
              mapped_col="AnnGrowth_07_12", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1, 3), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic population", subtext="2007-2012")
plotfunGrowth(map_data=popn_data, 
              mapped_col="AnnGrowth_12_17", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[4:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic population", subtext="2012-2017")

#####################
#OIL AND GAS ----
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_oilandgas2")
#read in table
tbl <- read.csv(paste0(wd, "/Tables/Oilandgas_wells_summarystats.csv"))
#manipulate table
tbl_ed <- tbl[tbl$Metric=="All", ] #both development and exploration wells
#merge table to sf
wells_data <- merge(template, tbl_ed, by=c("Country", "Region"), all.x=TRUE)

#plot
plotfunIntensity(map_data=wells_data, 
                 mapped_col="Intensity", 
                 mycols = rev(viridisLite::magma(8)[2:7]),
                 pretty_breaks=c(100, 500, 1000, 5000), ndecimals=0, 
                 legendtitle="Number of wells drilled", 
                 titletext="Arctic Oil and Gas", subtext="Intensity", caption="Any development or exploration wells drilled after 1960")
plotfunGrowth(map_data=wells_data, 
              mapped_col="AnnGrowth_start_end", 
              mycols = c("grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(1, 3, 5, 7), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic Oil and Gas", subtext="All years")
plotfunGrowth(map_data=wells_data, 
              mapped_col="AnnGrowth_02_07", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1, 3), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic Oil and Gas", subtext="2002-2007")
plotfunGrowth(map_data=wells_data, 
              mapped_col="AnnGrowth_07_12", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[3:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1, 3), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic Oil and Gas", subtext="2007-2012")
plotfunGrowth(map_data=wells_data, 
              mapped_col="AnnGrowth_12_17", 
              mycols = c("grey30","grey50", "grey70",rev(viridisLite::viridis(7)[4:6])),
              pretty_breaks=c(-3, -0.5, 0.5, 1), ndecimals=3, 
              legendtitle="Annual growth, percent", 
              titletext="Arctic Oil and Gas", subtext="2012-2017")

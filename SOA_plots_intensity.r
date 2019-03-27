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
#soa country borders for no data
nodata <- read_sf(dsn=datawd, layer="SOA_borders_for_NODATA")
#AMAP borders
amap <- read_sf(dsn=datawd, layer="AMAP_updatedRussia_clipto60N_SOA")

#metrics <- c("Cruise_tourism", "Domestic_tourism", "International_tourism", "Population", "Shipping_distance", "Shipping_volume", "Hunters", "Fishing", "Oilandgas_wells")

#read in table
tbl <- read.csv(paste0(wd, "/Analysis/Output/All_industries_intensity_perkm.csv"))
tbl <- tbl %>% select(-one_of("Country.x", "Country.y"))

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
        #geom_sf(data=soaborders, fill="grey85", color=NA) + #add countyborders
        geom_sf(data=nodata, fill="grey85", color=NA) + #add polygons for no data
        geom_sf(data=map_data, aes(fill=brks), color=NA, show.legend=TRUE) +
        labs(x = NULL, y = NULL, 
             title = titletext,
             subtitle = subtext, ...)+
        coord_sf(crs = st_crs(map_data), datum = NA, expand=FALSE) + #drop graticules
        theme(title=element_text(size=18), legend.text=element_text(size=14), legend.title=element_text(16),
              panel.grid=element_blank(), legend.position="bottom", 
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
  outname <- gsub("\\.", "", outname)
  outname <- paste0("Figures/", gsub(" ", "_", outname), ".png")
  png(outname, height=6, width=4.41, units="in", res=300)
  print(p)
  dev.off()
  
  #outname <- paste0("Figures/", gsub(" ", "_", outname), ".pdf")
  #pdf(outname, height=6, width=4.41, units="in")
  #print(p)
  #dev.off()
}

###########################
#MAIN PROCESSING ----
###########################
#####################
#Combined TOURISM
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_tourism3")

  #manipulate table
  tbl_ed <- tbl %>% filter(subIndustry %in% c("International_tourism", "Domestic_tourism")) %>% 
                             select(Region, intensity, adj_intensity, Area_km) %>%
                             group_by(Region) %>%
                             summarize(Area_km=mean(Area_km),
                                       intensity_thou=sum(intensity)/1000,
                                       adj_intensity=sum(adj_intensity))
    #merge table to sf
  tourist_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)

  #plot Density
  plotfunIntensity(map_data=tourist_data, 
                   mapped_col="adj_intensity",
                   mycols = c("lightcyan2", rev(viridisLite::magma(8)[2:6])),
                   pretty_breaks=c(5, 30, 50, 100), ndecimals=0, 
                   legendtitle=expression(Tourists~per~km^{2}), 
                   titletext="A. Tourism", subtext="")
                   #titletext="Arctic tourism", subtext="Density", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  
  #plot Intensity
  plotfunIntensity(map_data=tourist_data, 
                   mapped_col="intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(30, 100, 300, 1000, 3000), ndecimals=0, 
                   legendtitle="Tourists, thousands", 
                   titletext="Arctic tourism", subtext="Intensity", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  
  
#####################
#CRUISE TOURISM ----
#manipulate table
  tbl_ed <- tbl %>% filter(subIndustry=="Cruise_tourism" & !Region=="Murmansk") %>% select(Region, intensity, adj_intensity)
  #merge table to sf
  cruise_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)
  cruise_data$intensity_thou <- cruise_data$intensity/1000
  
#plot
plotfunIntensity(map_data=cruise_data, 
          mapped_col="intensity_thou", 
          mycols = rev(viridisLite::magma(8)[3:7]),
          pretty_breaks=c(1, 5, 10, 100), ndecimals=0, 
          legendtitle="Annual passengers, thousands", 
          titletext="Arctic cruise tourism", subtext="Intensity")
#plot
plotfunIntensity(map_data=cruise_data, 
                 mapped_col="adj_intensity", 
                 mycols = rev(viridisLite::magma(8)[3:7]),
                 pretty_breaks=c(5, 10, 15), ndecimals=0, 
                 legendtitle="Annual passengers/km coastline", 
                 titletext="Arctic cruise tourism", subtext="Density")

#####################
#DOMESTIC TOURISM ----
#manipulate table
tbl_ed <- tbl %>% filter(subIndustry=="Domestic_tourism") %>% select(Region, intensity, adj_intensity)

  #merge table to sf
  domestic_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)
  domestic_data$intensity_thou <- domestic_data$intensity/1000
  
  #plot
  plotfunIntensity(map_data=domestic_data, 
                   mapped_col="intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(20, 100, 200, 500, 1000), ndecimals=0, 
                   legendtitle="Domestic tourists, thousands", 
                   titletext="Arctic domestic tourism", subtext="Intensity", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  #plot
  plotfunIntensity(map_data=domestic_data, 
                   mapped_col="adj_intensity",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(5, 10, 15, 20), ndecimals=0, 
                   legendtitle=expression(Domestic~tourists/km^{2}), 
                   titletext="Arctic domestic tourism", subtext="Density", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  
  
#####################  
#INTERNATIONAL TOURISM ----
  #manipulate table
  tbl_ed <- tbl %>% filter(subIndustry=="International_tourism") %>% select(Region, intensity, adj_intensity)
  #merge table to sf
  international_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)
  international_data$intensity_thou <- international_data$intensity/1000
  
  #plot
  plotfunIntensity(map_data=international_data, 
                   mapped_col="intensity_thou",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(10, 50, 150, 500, 1000), ndecimals=0, 
                   legendtitle="International tourists, thousands", 
                   titletext="Arctic international tourism", subtext="Intensity", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")

  #plot
  plotfunIntensity(map_data=international_data, 
                   mapped_col="adj_intensity",
                   mycols = rev(viridisLite::magma(8)[2:7]),
                   pretty_breaks=c(5, 15, 25, 70), ndecimals=0, 
                   legendtitle=expression(International~tourists/km^{2}), 
                   titletext="Arctic international tourism", subtext="Density", caption="Canada, Alaska, Russia = number of visitors; \nAll other countries = guest nights")
  
  

  
#####################
#OIL AND GAS All----
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_oilandgas4")
  #manipulate table
  tbl_ed <- tbl %>% filter(Industry =="OilandGas") %>% 
    select(Country, Region, intensity, adj_intensity, Area_km) %>% #both development and exploration wells
    group_by(Country, Region) %>%
    summarize(Area_km=mean(Area_km),
              intensity=sum(intensity),
              adj_intensity=sum(adj_intensity)) %>% ungroup()
  #add a row for the Faroes - I found this data manually
  tbl_ed <- add_row(tbl_ed, Country="Faroe Islands", Region="FaroesEEZ",Area_km=212113.841213,
                     intensity=9, adj_intensity=10000*9/212113.841213)
  #add a row for the 1970s Nunuvut drilling - I found this data manually
  tbl_ed <- add_row(tbl_ed, Country="Canada", Region="Northern Canadian Archipelago", Area_km=464922.707407,
                     intensity=180, adj_intensity=10000*180/464922.707407)

  
#merge table to sf
wells_data <- merge(template, tbl_ed, by=c("Country", "Region"), all.x=TRUE)

#plot Density
plotfunIntensity(map_data=wells_data, 
                 mapped_col="adj_intensity", 
                 mycols = c("lightcyan2", rev(viridisLite::magma(8)[2:6])),
                 pretty_breaks=c(0.5, 2, 5, 10, 30), ndecimals=1, 
                 legendtitle=expression(paste(Wells~per~{10}, ",000", ~km^{2})), 
                 titletext="C. Oil and gas", subtext="")
                 #titletext="Arctic oil and gas", subtext="Density", caption="Any development or exploration wells drilled after 1960")

#plot Intensity
plotfunIntensity(map_data=wells_data, 
                 mapped_col="intensity", 
                 mycols = rev(viridisLite::magma(8)[2:8]),
                 pretty_breaks=c(50, 100, 500, 1000, 5000), ndecimals=0, 
                 legendtitle="Number of wells drilled", 
                 titletext="Arctic oil and gas", subtext="Intensity", caption="Any development or exploration wells drilled after 1960")



########################
#OIL AND GAS Exploration----
#manipulate table
tbl_ed <- tbl %>% filter(subIndustry =="Oilandgas_Exploration") 
#merge table to sf
wells_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)

plotfunIntensity(map_data=wells_data, 
                 mapped_col="intensity", 
                 mycols = rev(viridisLite::magma(8)[4:8]),
                 pretty_breaks=c(50, 100, 500), ndecimals=0, 
                 legendtitle="Number of wells drilled", 
                 titletext="Arctic oil and gas", subtext="Exploration intensity", caption="Exploration wells drilled after 1960")

plotfunIntensity(map_data=wells_data, 
                 mapped_col="adj_intensity", 
                 mycols = rev(viridisLite::magma(8)[2:7]),
                 pretty_breaks=c(0.25, 0.5, 1), ndecimals=1, 
                 legendtitle=expression(Wells~per~{10000}~km^{2}), 
                 titletext="Arctic oil and gas", subtext="Exploration density", caption="Exploration wells drilled after 1960")


########################
#OIL AND GAS Development----
tbl_ed <- tbl %>% filter(subIndustry =="Oilandgas_Development")
#merge table to sf
wells_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)

plotfunIntensity(map_data=wells_data, 
                 mapped_col="intensity", 
                 mycols = rev(viridisLite::magma(8)[2:8]),
                 pretty_breaks=c(50, 100, 500, 1000, 5000), ndecimals=0, 
                 legendtitle="Number of wells drilled", 
                 titletext="Arctic oil and gas", subtext="Development intensity", caption="Development wells drilled after 1960")

plotfunIntensity(map_data=wells_data, 
                 mapped_col="adj_intensity", 
                 mycols = rev(viridisLite::magma(8)[2:7]),
                pretty_breaks=c(0.5, 1, 2, 3), ndecimals=1, 
                 legendtitle=expression(Wells~per~{10000}~km^{2}), 
                 titletext="Arctic oil and gas", subtext="Development density", caption="Development wells drilled after 1960")

#####################
#MINING ----
template <- read_sf(dsn=datawd, layer="SOA_borders_for_popn2") #we use this one because mining2 has kamchatcha below 60N

tbl_ed <- tbl %>% filter(subIndustry =="Mining")
#merge table to sf
mine_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)
#mine_data$adj_intensity <- mine_data$adj_intensity

#Plot density
plotfunIntensity(map_data=mine_data, 
                 mapped_col="adj_intensity", 
                 mycols = c("lightcyan2", rev(viridisLite::magma(8)[2:6])),
                 pretty_breaks=c(0.5, 1, 1.5, 2), ndecimals=1, 
                 legendtitle=expression(paste(Mines~per~{10}, ",000",~km^{2})), 
                 titletext="B. Mining", subtext="")
                 #titletext="Arctic mining", subtext="Density", caption="Any mines operating after 1960")

#plot intensity
plotfunIntensity(map_data=mine_data, 
                 mapped_col="intensity", 
                 mycols = rev(viridisLite::magma(8)[2:7]),
                 pretty_breaks=c(5, 10, 15, 20, 25), ndecimals=0, 
                 legendtitle="Number of mines", 
                 titletext="Arctic mining", subtext="Intensity", caption="Any mines operating after 1960")



#####################
#FISHING ----
#read in template
template <- read_sf(dsn=datawd, layer="SOA_borders_for_fishing")
nodata <- read_sf(dsn=datawd, layer="SOA_borders_for_NODATA_fishing")

tbl_ed <- tbl %>% filter(Industry =="Fishing")


#merge table to sf
fishing_data <- merge(template, tbl_ed, by=c("Region"), all=TRUE)
#add a row for the central arctic
#fishing_data[fishing_data$Region=="Central Arctic", c("intensity", "adj_intensity")] <- NA 
fishing_data$intensity_thou <- fishing_data$intensity/1000

#plot density
plotfunIntensity(map_data=fishing_data, 
                 mapped_col="adj_intensity", 
                 mycols = c("lightcyan2", rev(viridisLite::magma(8)[2:6])),
                 pretty_breaks=c(0.2, 0.5, 1, 2), ndecimals=1, 
                 legendtitle=expression(Tonnes~catch~per~km^{2}), 
                 titletext="D. Fisheries", subtext="")
                 #titletext="Arctic fishing catch", subtext="Density")

#plot intensity
plotfunIntensity(map_data=fishing_data, 
                 mapped_col="intensity_thou", 
                 mycols = rev(viridisLite::magma(8)[2:8]),
                 pretty_breaks=c(1, 10, 100, 500, 1000), ndecimals=0, 
                 legendtitle="Annual catch, thousand tonnes", 
                 titletext="Arctic fishing catch", subtext="Intensity")



#####################
#SHIPPING - ALL ----
#read in template
template <- read_sf(dsn=datawd, layer="EEZ_plus_highseas_noland")
nodata <- read_sf(dsn=datawd, layer="SOA_borders_for_NODATA_shipping")

#manipulate table
tbl_ed <- tbl %>% filter(Industry =="Shipping_distance") %>% droplevels()
#merge table to sf
shipping_data <- merge(template, tbl_ed, by=c("Region"), all.x=TRUE)
shipping_data$intensity_thou <- shipping_data$intensity/100000

#plot density
plotfunIntensity(map_data=shipping_data, 
                 mapped_col="adj_intensity", 
                 mycols = c("lightcyan2", rev(viridisLite::magma(8)[2:6])),
                 pretty_breaks=c(1, 2, 4, 6), ndecimals=0, 
                 legendtitle=expression(Annual~nm~sailed~per~km^{2}), 
                 titletext="E. Shipping", subtext="")
                 #titletext="Arctic shipping distance", subtext="Density")

#plot intensity
plotfunIntensity(map_data=shipping_data, 
                 mapped_col="intensity_thou", 
                 mycols = rev(viridisLite::magma(8)[2:8]),
                 pretty_breaks=c(3, 10, 25, 50, 150), ndecimals=0, 
                 legendtitle=expression(Annual~distance~sailed~(x~10^{5}~nm)), 
                 titletext="Arctic shipping distance", subtext="Intensity")



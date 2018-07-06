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
borders <- read_sf(paste0(dsn=dirname(datawd), "/60degreesN"), layer="CountryBorders_45degreesN_lambert_noAMAP_1000kmenvelope")
#AMAP borders
amap <- read_sf(paste0(dsn=dirname(datawd), "/AMAP"), layer="AMAP_updatedRussia_clipto60N_SOA")

#metrics <- c("Cruise_tourism", "Domestic_tourism", "International_tourism", "Population", "Shipping_distance", "Shipping_volume", "Hunters", "Fishing", "Oilandgas_wells")


#############
#Set up functions ----
#function to add the min value on the legend and change length of bars
#draws on code from 
#https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#a-very-basic-map
extendLegendWithExtremes <- function(p){
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

plotfunGrowth <- function(map_data, mapped_col, pretty_breaks, mycols, ndecimals, legendtitle, titletext, subtext) {
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
        geom_sf(data=map_data, aes(fill=brks), color=NA, show.legend=TRUE) +
        labs(x = NULL, y = NULL, 
        title = titletext,
        subtitle = subtext) +
        #caption = "")
        coord_sf(crs = st_crs(map_data), datum = NA, expand=FALSE) + #drop graticules
        theme(panel.grid=element_blank(), legend.position="bottom", 
              panel.background = element_rect(fill="grey90"), 
              plot.background = element_rect(fill = "grey90", color="grey90"),
              legend.background = element_rect(fill="grey90"),
              plot.margin = unit(c(0,0,0,0), "pt")) +
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
  extendLegendWithExtremes(p)
  dev.off()
}

plotfunIntensity <- function(map_data, mapped_col, pretty_breaks, ndecimals, legendtitle, titletext, subtext) {
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
        geom_sf(data=map_data, aes(fill=brks), color=NA, show.legend=TRUE) +
        labs(x = NULL, y = NULL, 
             title = titletext,
             subtitle = subtext) +
        #caption = "")
        coord_sf(crs = st_crs(map_data), datum = NA, expand=FALSE) + #drop graticules
        theme(panel.grid=element_blank(), legend.position="bottom", 
              panel.background = element_rect(fill="grey90"), 
              plot.background = element_rect(fill = "grey90", color="grey90"),
              legend.background = element_rect(fill="grey90"),
              plot.margin = unit(c(0,0,0,0), "pt")) +
        scale_fill_manual(values = rev(viridisLite::magma(8)[3:7]), breaks = rev(brks_scale),
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
  p
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

c("AnnGrowth_start_end", "AnnGrowth_02_07","AnnGrowth_07_12", "AnnGrowth_12_17", "Intensity")

plotfunIntensity(map_data=cruise_data, 
          mapped_col="Intensity_thou", 
          pretty_breaks=c(1, 5, 10, 100), ndecimals=0, 
          legendtitle="Annual passengers, thousands", 
          titletext="Arctic cruise tourism", subtext="Intensity")

  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_start_end", 
          mycols = c(rev(viridisLite::viridis(8)[3:7])),
          pretty_breaks=c(5, 10, 15, 35), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="All years")
  
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_02_07", 
          mycols = c(rev(viridisLite::viridis(8)[2]), "grey70", rev(viridisLite::viridis(8)[3:6])),
          pretty_breaks=c(-1, 1, 5, 10, 15), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2002-2007")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_07_12", 
          mycols = c(rev(viridisLite::viridis(8)[4:7])),
          pretty_breaks=c(5, 10, 15), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2007-2012")
  plotfunGrowth(map_data=cruise_data, 
          mapped_col="AnnGrowth_12_17", 
          mycols = c(rev(viridisLite::viridis(8)[2]), "grey70", rev(viridisLite::viridis(8)[3:7])),
          pretty_breaks=c(-1, 1, 5, 10, 15, 35), ndecimals=3, 
          legendtitle="Annual growth, percent", 
          titletext="Arctic cruise tourism", subtext="2012-2017")






  p + theme(panel.background = element_rect(fill="grey90"), 
            plot.background = element_rect(fill = "grey90", color="grey90"),
            legend.background = element_rect(fill="grey90"),
            plot.margin = unit(c(0,0,0,0), "pt")) 




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








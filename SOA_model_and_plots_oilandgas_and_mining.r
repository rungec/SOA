require(knitr)
require(tidyverse)
require(lubridate)
require(itsadug) #for gam visualisation
require(mgcv) #for gam mixed effects models
require(broom) #augment
require(modelr)
require(gridExtra) #for grid.arrange
#require(ggfortify) #for autoplot
require(pander) #for pander table
require(ggpubr) #for ggarrange

##########################
# Set directories ----
##########################

#wd = "C:/Users/cru016/Documents/CONNECT/Paper_5_state_of_arctic/Analysis/Intermediate"
wd = "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis/Model"
setwd(wd)

##########################
### Set up data ----
##########################

alldat <- read.csv(paste0(dirname(wd), "/output/All_industries_long.csv"), fileEncoding = "UTF-8-BOM")

#Oil and gas: all data is in number of wells drilled (apart from Russia) and we model Russia separately
oag <- alldat %>% filter(Industry=="OilandGas") %>% 
        mutate(Country = as.factor(case_when(Country %in% c("Norway", "HighSeas", "Svalbard", "Jan Mayen") ~ "Norway", 
                                             Country == "Alaska" ~ "Alaska",
                                             Country == "Canada" ~ "Canada",
                                             Country == "Greenland" ~ "Greenland"))) %>%
        mutate(Offshore = case_when(Region %in% c("Barents Sea", "Beaufort Sea", "Chukchi Sea", "Greenland offshore", "GreenlandEEZ", 
                                                  "JanMayenEEZ", "Newfoundland-Labrador Shelf", 
                                                  "Norwegian Sea", "OpenBarentsSea", "OpenNorwegianSea", 
                                                  "Svalbard offshore", "West Bering Sea") ~ TRUE,
                                    Region %in% c("Alaska", "Greenland", "Northwest Territories", "Yukon") ~ FALSE)) %>%
        filter(!Country=="United Kingdom") %>%
        filter(!year==0) %>%
        filter(year >= 1950 & year <=2017) %>%
        droplevels() 



##########################
### Set up plotting functions ----
##########################

modelsummaryfun <- function(i, m1, m2, currdf, ...){
  
  #plot the model autocorrelation and acf plot
    png(paste0(i, "_modelcomparison_ACFplots.png"), width=7, height=7, units="in", res=300)
      par(mfrow=c(2,2))
      acf(resid(m1$lme, type="normalized"), lag=20, main="ACF")
      pacf(resid(m1$lme, type="normalized"), lag=20, main="PACF")
      acf(resid(m2$lme, type="normalized"), lag=20, main="ACF")
      pacf(resid(m2$lme, type="normalized"), lag=20, main="PACF")
    dev.off()
    
    sink(paste0(i, "_allregions_summarystats.txt"))
    print("model 1 (may have temporal autocorrelation)")
    print(summary(m1$lme))
    print(summary(m1$gam))
    print("model 2 (errors adjusted for AR(1) temporal autocorrelation)")
    print(summary(m2$lme))
    print(summary(m2$gam))
    print(anova(m2$lme, m1$lme)) #any difference in the random effects?
    png(paste0(i, "_allregions_model2_fit.png"), width=7, height=7, units="in", res=300)
      par(mfrow=c(2,2))
      print(gam.check(m2$gam))
    dev.off()
    sink()
    
    #Plot the model with and without temporal autocorrelation
    png(paste0(i, "_modelcomparison_fit.png"), width=14, height=7, units="in", res=300)
    par(mfrow=c(1,2))
    plot(m1$gam, residuals = TRUE, pch = 19, cex = 0.75, shade = TRUE, rug = FALSE, main="With temporal autocorrelation", seWithMean = TRUE)
    plot(m2$gam, residuals = TRUE, pch = 19, cex = 0.75, shade = TRUE, rug = FALSE, main="AR(1) removed", seWithMean = TRUE)
    dev.off()
}

bestmodplot <- function(bestmod, currdf, bycountry, pwidth, pheight){
      if (bycountry==FALSE){
        preds <- predict(bestmod$gam, newdata=currdf, se=TRUE)
        preds <- bind_cols(currdf, preds)
        p <-  preds %>%  
          ggplot(aes(x=year, y=nwells)) +
          geom_point(col="black") +
          geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
          geom_line(aes(y=fit), col="black") +
          theme(legend.position="none") +
          theme_minimal()
        ggsave(filename=paste0(i, "_timeseries.png"), p, width = pwidth, height=pheight)
     
       } else if (bycountry==TRUE) {
        #Plot the model by region
        preds <- predict(bestmod$gam, newdata=currdf, se=TRUE)
        preds <- bind_cols(currdf, preds)
        p <-  preds %>%  
          ggplot(aes(x=year, y=znorm)) +
          geom_point(col="black") +
          geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
          geom_line(aes(y=fit), col="black") +
          facet_wrap(~Country) +
          theme(legend.position="none") +
          theme_minimal()
        ggsave(filename=paste0(i, "_timeseries_bycountry.png"), p, width = pwidth, height=pheight)
      }
}

##########################
# OIL and GAS Models ----
##########################
### Models using 'GAMM' function
#We test 2 models for each industry:  
#mod1: A gamm model of z-normalised value ~ smoothed year with Region as a random effect. We look at the ACF plots and Ljung-Box test to see if there is any significant temporal autocorrelation (remembering that by chance we expect 1 out of every 20 sample autocorrelations to exceed the 95% bounds (the blue dotted lines)) 
#mod2 = mod1 with autoregressive process of order 1

##########################
#Model oil and gas exploration, all regions except russia 1950-2017 ----
i <- "Oilandgas_Exploration_allregions"
#Set up normalised data
oag_exp <- oag %>% filter(subIndustry=="Oilandgas_Exploration") %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_exp)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_exp)

modelsummaryfun(i, mod1, mod2, oag_exp)
  
##########################
#Model oil and gas development, all regions  except russia 1950-2017 ----
i <- "Oilandgas_Development_allregions"
#Set up normalised data
oag_dev <- oag %>% filter(subIndustry=="Oilandgas_Development") %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_dev)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_dev)

modelsummaryfun(i, mod1, mod2, oag_dev)

##########################
#Model offshore exploration, all regions except russia 1970-2017 ----
#excludes data for Faroes & Nunavut as we don't have dates for when these wells were drilled
i <- "Oilandgas_Exploration_offshore"
#Set up normalised data
oag_exp_off <- oag %>% filter(subIndustry=="Oilandgas_Exploration" & Offshore==TRUE) %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_exp_off)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_exp_off)

modelsummaryfun(i, mod1, mod2, oag_exp_off)

##########################
#Model offshore development, all regions  except russia 1970-2017 ----
i <- "Oilandgas_Development_offshore"
#Set up normalised data
oag_dev_off <- oag %>% filter(subIndustry=="Oilandgas_Development" & Offshore==TRUE) %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_dev_off)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_dev_off)

modelsummaryfun(i, mod1, mod2, oag_dev_off)

##########################
#Model oil and gas exploration, aggregated all regions except russia 1950-2017 ----
i <- "Oilandgas_Exploration_aggregated"
#Set up normalised data
oag_exp_agg <- oag %>% filter(subIndustry=="Oilandgas_Exploration") %>%
  group_by(year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod1 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_exp_agg)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(nwells ~ s(year), correlation = corAR1(form = ~ year), data=oag_exp_agg)

modelsummaryfun(i, mod1, mod2, oag_exp_agg)
bestmodplot(mod1, oag_exp_agg, FALSE, 9, 7)
preds <- predict(mod1$gam, newdata=oag_exp_agg, se=TRUE)
oag_exp_agg_df <- bind_cols(oag_exp_agg, preds) %>% mutate(subIndustry="Exploration")

##########################
#Model oil and gas development, aggregated all regions  except russia 1950-2017 ----
i <- "Oilandgas_Development_aggregated"
#Set up normalised data
oag_dev_agg <- oag %>% filter(subIndustry=="Oilandgas_Development") %>%
  group_by(year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod1 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_dev_agg)
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(nwells ~ s(year), correlation = corAR1(form = ~ year), data=oag_dev_agg)

modelsummaryfun(i, mod1, mod2, oag_dev_agg)
bestmodplot(mod2, oag_exp_agg, FALSE, 9, 7)
preds <- predict(mod1$gam, newdata=oag_dev_agg, se=TRUE)
oag_dev_agg_df <- bind_cols(oag_dev_agg, preds) %>% mutate(subIndustry="Development")

##########################
#Model oil and gas exploration, aggregated offshore except russia 1950-2017 ----
i <- "Oilandgas_Exploration_aggregated_offshore"
#Set up normalised data
oag_exp_agg_off <- oag %>% filter(subIndustry=="Oilandgas_Exploration" & Offshore==TRUE) %>%
  group_by(year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod1 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_exp_agg_off)
preds <- predict(mod1$gam, newdata=oag_exp_agg_off, se=TRUE)
oag_exp_agg_off_df <- bind_cols(oag_exp_agg_off, preds) %>% mutate(subIndustry="Offshore Exploration")

##########################
#Model oil and gas development, aggregated offshore  except russia 1950-2017 ----
i <- "Oilandgas_Development_aggregated_offshore"
#Set up normalised data
oag_dev_agg_off <- oag %>% filter(subIndustry=="Oilandgas_Development" & Offshore==TRUE) %>%
  group_by(year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod1 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_dev_agg_off)
preds <- predict(mod1$gam, newdata=oag_dev_agg_off, se=TRUE)
oag_dev_agg_off_df <- bind_cols(oag_dev_agg_off, preds) %>% mutate(subIndustry="Offshore Development")


##########################
# PLOT OIL AND GAS TRENDS
##########################

#exploration vs development time series
oagplotdf <- bind_rows(oag_exp_agg_df, oag_dev_agg_df)
p <- ggplot(oagplotdf, aes(x=year, y=nwells, fill=subIndustry, group=subIndustry)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  geom_line(aes(y=fit, col=subIndustry)) +
  coord_cartesian(xlim=c(1950, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("Wells spudded") +
  theme_minimal() +
  theme(legend.position=c(0.22, 0.80)) +
  scale_color_manual(values = viridisLite::magma(4)[2:3], guide = "none") +
  scale_fill_manual(values = viridisLite::magma(4)[2:3],
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))
ggsave(filename=paste0("Arctic_Oil_and_Gas_Timeseries.png"), p)

#Marine timeseries
oagplotdfoff <- bind_rows(oag_exp_agg_off_df, oag_dev_agg_off_df)
p <- ggplot(oagplotdfoff, aes(x=year, y=nwells, fill=subIndustry, group=subIndustry)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  geom_line(aes(y=fit, col=subIndustry)) +
  coord_cartesian(xlim=c(1970, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1970, 2010, 10)) +
  xlab("Year") + ylab("Wells spudded") +
  theme_minimal() +
  theme(legend.position='bottom') +
  scale_color_manual(values = viridisLite::viridis(4)[2:3], guide = "none") +
  scale_fill_manual(values = (viridisLite::viridis(4)[2:3]),
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))

ggsave(filename=paste0("Arctic_Oil_and_Gas_Timeseries_marine.png"), p)



##########################
# MINING Models ----
##########################

#Model mines, all regions except russia 1950-2017

#Model mines, russia 1950-2017




##########################
# Plots for SI ----
##########################

#Plot number of wells spudded (all regions), overlaid with exploration and development trendlines (plus SI) from models






#Plot number of wells spudded offshore (all regions), overlaid with exploration and development trendlines (plus SI) from models


#Plot trendlines for mines active over time (1950s to present) by region 
#OR for Russia and Finland vs rest of Arctic







#exploration vs development time series
#read in table

p <- ggplot(tbl4, aes(x=year, y=nwells, fill=Metric)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  coord_cartesian(xlim=c(1950, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("Wells spudded") +
  theme_minimal() +
  theme(legend.position=c(0.22, 0.80)) +
  scale_fill_manual(values = viridisLite::magma(4)[2:3],
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))
ggsave(filename=paste0("Figures/", "Arctic_Oil_and_Gas_Timeseries.png"), p)

#Marine timeseries
#dropped norwegian north sea
tbl5 <- tbl2 %>% filter(Metric!="All" & Region %in% c("Labrador offshore", "Norwegian Sea", 
                                                      "Barents Sea", "Offshore", "West Bering Sea", "Chukchi Sea", "Beaufort Sea")) %>% 
  group_by(Metric, year) %>% summarise(nwells=sum(value, na.rm=TRUE))

p <- ggplot(tbl5, aes(x=year, y=nwells, fill=Metric)) +
  geom_area(stat='identity', position='stack', alpha=0.5) +
  geom_line(tbl6, aes(x=year, y=nwells)) +
  coord_cartesian(xlim=c(1970, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1970, 2010, 10)) +
  xlab("Year") + ylab("Wells spudded") +
  theme_minimal() +
  theme(legend.position='bottom') +
  scale_fill_manual(values = (viridisLite::viridis(4)[2:3]),
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))

ggsave(filename=paste0("Figures/", "Arctic_Oil_and_Gas_Timeseries_marine.png"), p)

#####################
#MINING PLOTS ----

#Plot number of mines operating in each year
#load and summarise data
mines <- read.csv("Analysis/Intermediate/Mining_long.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
#names(mines)[1] <- "Country"
plotdfOP <- mines %>% filter(Metric=="Operation") %>% group_by(Country, year) %>% summarise(nmines=sum(value, na.rm=TRUE))
#change order of levels
plotdfOP$Country <- factor(plotdfOP$Country, levels=rev(c("Greenland", "Canada", "USA", "Finland", "Norway", "Sweden", "Russia")))

#Plot one plot
p <- ggplot(plotdfOP, aes(x=year, y=nmines, fill=Country, col=Country)) + 
  geom_area(stat='identity', position='identity', alpha=0.6) +
  geom_line(alpha=0.6) +
  coord_cartesian(xlim=c(1900, 2017), expand=FALSE) + 
  #scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("Number of mines in operation") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_fill_manual(values = rev(viridisLite::magma(9)[2:8]),
                    name = element_blank(), 
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30/length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = F, label.position = "bottom")) +
  scale_colour_manual(values = rev(viridisLite::magma(9)[2:8]), guide = "none")
ggsave(filename=paste0("Figures/", "Arctic_Mining_Timeseries_bycountry_operational.png"), p)

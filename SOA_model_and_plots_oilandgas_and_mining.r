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

#Mining: all data is in number of active mines
mines <- alldat %>% filter(Industry=="Mining" & Metric!="Exploration") %>% 
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
    #print(anova(m2$lme, m1$lme)) #any difference in the random effects? ##This is not accurate for models with and without AR1
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
          theme_minimal(18)
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
          theme_minimal(18)
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

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_exp, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_exp, method="REML")

modelsummaryfun(i, mod1, mod2, oag_exp)
bestmodplot(mod2, oag_exp, TRUE, 9, 7)
  
##########################
#Model oil and gas development, all regions  except russia 1950-2017 ----
i <- "Oilandgas_Development_allregions"
#Set up normalised data
oag_dev <- oag %>% filter(subIndustry=="Oilandgas_Development") %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% droplevels() %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod3 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_dev, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod4 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_dev, method="REML")

modelsummaryfun(i, mod3, mod4, oag_dev)
bestmodplot(mod3, oag_dev, TRUE, 12, 5)

##########################
#Model offshore exploration offshore, all regions except russia 1970-2017 ----
#excludes data for Faroes & Nunavut as we don't have dates for when these wells were drilled
i <- "Oilandgas_Exploration_offshore"
#Set up normalised data
oag_exp_off <- oag %>% filter(subIndustry=="Oilandgas_Exploration" & Offshore==TRUE) %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod5 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=oag_exp_off, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod6 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=oag_exp_off, method="REML")

modelsummaryfun(i, mod5, mod6, oag_exp_off)
bestmodplot(mod5, oag_exp_off, TRUE, 9, 7)

##########################
#Model offshore development offshore, all regions  except russia 1970-2017 ----
i <- "Oilandgas_Development_offshore"
#Set up normalised data
oag_dev_off <- oag %>% filter(subIndustry=="Oilandgas_Development" & Offshore==TRUE) %>%
  group_by(Country, year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nwells, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

#only one country, so we drop country from the model
mod7 <- gamm(znorm ~ s(year), correlation=NULL, data=oag_dev_off, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod8 <- gamm(znorm ~ s(year), correlation = corAR1(form = ~ year), data=oag_dev_off, method="REML")

modelsummaryfun(i, mod7, mod8, oag_dev_off)
bestmodplot(mod7, oag_dev_off, TRUE, 9, 7)

##########################
#Model oil and gas exploration, aggregated all regions except russia 1950-2017 ----
i <- "Oilandgas_Exploration_aggregated"
#Set up normalised data
oag_exp_agg <- oag %>% filter(subIndustry=="Oilandgas_Exploration") %>%
  group_by(year) %>%
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup()

mod9 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_exp_agg, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod10 <- gamm(nwells ~ s(year), correlation = corAR1(form = ~ year), data=oag_exp_agg, method="REML")

modelsummaryfun(i, mod9, mod10, oag_exp_agg)
bestmodplot(mod9, oag_exp_agg, FALSE, 9, 7)
preds <- predict(mod9$gam, newdata=oag_exp_agg, se=TRUE)
oag_exp_agg_df <- bind_cols(oag_exp_agg, preds) %>% mutate(subIndustry="Exploration")

##########################
#Model oil and gas development, aggregated all regions  except russia 1950-2017 ----
i <- "Oilandgas_Development_aggregated"
#Set up normalised data
oag_dev_agg <- oag %>% filter(subIndustry=="Oilandgas_Development") %>%
  group_by(year) %>% 
  summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod11 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_dev_agg, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod12 <- gamm(nwells ~ s(year), correlation = corAR1(form = ~ year), data=oag_dev_agg, method="REML")

modelsummaryfun(i, mod11, mod12, oag_dev_agg)
bestmodplot(mod12, oag_exp_agg, FALSE, 9, 7)
preds <- predict(mod11$gam, newdata=oag_dev_agg, se=TRUE)
oag_dev_agg_df <- bind_cols(oag_dev_agg, preds) %>% mutate(subIndustry="Development")

##########################
#Model oil and gas exploration, aggregated offshore except russia 1950-2017 ----
 i <- "Oilandgas_Exploration_aggregated_offshore"
#Set up normalised data
 oag_exp_agg_off <- oag %>% filter(subIndustry=="Oilandgas_Exploration" & Offshore==TRUE) %>%
   group_by(year) %>% 
   summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 
 
mod13 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_exp_agg_off, method="REML")
preds <- predict(mod13$gam, newdata=oag_exp_agg_off, se=TRUE)
oag_exp_agg_off_df <- bind_cols(oag_exp_agg_off, preds) %>% mutate(subIndustry="Marine exploration")
 
##########################
# #Model oil and gas development, aggregated offshore  except russia 1950-2017 ----
 i <- "Oilandgas_Development_aggregated_offshore"
# #Set up normalised data
 oag_dev_agg_off <- oag %>% filter(subIndustry=="Oilandgas_Development" & Offshore==TRUE) %>%
   group_by(year) %>% 
   summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() 

mod14 <- gamm(nwells ~ s(year), correlation=NULL, data=oag_dev_agg_off, method="REML")
preds <- predict(mod14$gam, newdata=oag_dev_agg_off, se=TRUE)
oag_dev_agg_off_df <- bind_cols(oag_dev_agg_off, preds) %>% mutate(subIndustry="Marine development")


##########################
# PLOT OIL AND GAS TRENDS for SI ----
##########################

#Plot number of wells spudded (all regions), overlaid with exploration and development trendlines (plus SI) from models
#exploration vs development time series
oagplotdf <- bind_rows(oag_exp_agg_df, oag_dev_agg_df)
p <- ggplot(oagplotdf, aes(x=year, y=nwells, fill=subIndustry, group=subIndustry)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  #geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  #geom_line(aes(y=fit, col=subIndustry)) +
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
ggsave(filename=paste0("Arctic_Oil_and_Gas_Timeseries.png"), p, width=7.77, height=4.35)

#Plot number of wells spudded offshore (all regions), overlaid with exploration and development trendlines (plus SI) from models
#Marine timeseries
oagplotdfoff <- bind_rows(oag_exp_agg_off_df, oag_dev_agg_off_df)
p <- ggplot(oagplotdfoff, aes(x=year, y=nwells, fill=subIndustry, group=subIndustry)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  #geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  #geom_line(aes(y=fit, col=subIndustry)) +
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

ggsave(filename=paste0("Arctic_Oil_and_Gas_Timeseries_marine.png"), p, width=7.77, height=4.35)

##########################
# #Set up data aggregated all regions  except russia 1950-2017 ----
# oag_agg <- oag %>% 
#   group_by(subIndustry, year) %>% 
#   summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() %>% 
#   mutate(industry=case_when(subIndustry=="Oilandgas_Development"~"Development",
#                             subIndustry=="Oilandgas_Exploration"~"Exploration")) %>% ungroup()
# 
# #Set up data aggregated offshore  except russia 1950-2017 ----
# oag_agg_off <- oag %>% filter(Offshore==TRUE) %>%
#   group_by(subIndustry, year) %>% 
#   summarise(nwells=sum(value_raw, na.rm=TRUE)) %>% ungroup() %>% 
#   mutate(industry=case_when(subIndustry=="Oilandgas_Development"~"Marine development",
#                             subIndustry=="Oilandgas_Exploration"~"Marine exploration"))

#Plot normalised number of wells spudded (all regions), overlaid with exploration and development trendlines (plus SI) from models
#Set up predictions for trendlines estimated with country as random effect
#predict oag exploration
preds1 <- predict(mod2$gam, newdata=oag_exp, se=TRUE) 
preds1 <- bind_cols(oag_exp, preds1) %>% mutate(industry="Exploration")
#predict oag development
preds3 <- predict(mod3$gam, newdata=oag_dev, se=TRUE) 
preds3 <- bind_cols(oag_dev, preds3) %>% mutate(industry="Development")
#predict oag exploration offshore
preds5 <- predict(mod5$gam, newdata=oag_exp_off, se=TRUE) 
preds5 <- bind_cols(oag_exp_off, preds5) %>% mutate(industry="Marine exploration")
#predict oag development offshore
preds7 <- predict(mod7$gam, newdata=oag_dev_off, se=TRUE) 
preds7 <- bind_cols(oag_dev_off, preds7) %>% mutate(industry="Marine development")

oagplotdf <- bind_rows(preds1, preds3)
oagplotdf <- bind_rows(oagplotdf, preds5)
oagplotdf <- bind_rows(oagplotdf, preds7)

#plot exploration vs development time series
p <- ggplot(oagplotdf, aes(x=year, y=znorm, fill=industry, group=industry)) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.5)+
  geom_line(aes(y=fit, col=industry)) +
  coord_cartesian(xlim=c(1950, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("znorm") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_manual(values = viridisLite::magma(6)[2:5], guide = "none") +
  scale_fill_manual(values = viridisLite::magma(6)[2:5],
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = F, label.position = "bottom"))
ggsave(filename=paste0("Arctic_Oil_and_Gas_Timeseries_znorm.png"), p, width=7, height=5)

##########################
# MINING Models ----
##########################

#Model mines, all regions 1950-2017
i <- "Mining_allregions"
#Set up normalised data
mines_all <- mines %>% 
  group_by(Country, year) %>% 
  summarise(nmines=sum(value_raw, na.rm=TRUE)) %>%
  mutate(znorm = scale(nmines, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup() #### Normalisation

mod1 <- gamm(nmines ~ s(year), random=list(Country=~1), correlation=NULL, data=mines_all, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(nmines ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=mines_all, method="REML")

modelsummaryfun(i, mod1, mod2, mines_all)

#Model mines, all regions 1950-2017 normalised
i <- "Mining_allregions_znorm"

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=mines_all, method="REML")
#Add in temporal autocorrelation with 1yr lag
#mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corARMA(form = ~ year, p=10, q=0), data=mines_all)
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=mines_all, method="REML")

modelsummaryfun(i, mod1, mod2, mines_all)
bestmodplot(mod2, mines_all, TRUE, 9, 7)

#Model mines, all regions except russia aggregated 1950-2017 normalised
i <- "Mining_no_russia_znorm"
#Set up normalised data
mines_noruss <- mines_all %>% filter(Country!="Russia") 

mod1 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation=NULL, data=mines_noruss, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Country=~1), correlation = corAR1(form = ~ year), data=mines_noruss, method="REML")

modelsummaryfun(i, mod1, mod2, mines_noruss)
bestmodplot(mod2, mines_noruss, TRUE, 9, 7)

#Model mines, russia 1950-2017
i <- "Mining_russia_znorm"
#Set up normalised data
mines_russ <- mines_all %>% filter(Country=="Russia") 

mod1 <- gamm(znorm ~ s(year), correlation=NULL, data=mines_russ, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), correlation = corARMA(form = ~ year, p=1, q=0), data=mines_russ, method="REML")
#looks like we can do a lm
mod3 <- lm(znorm ~ year, data=mines_russ)

modelsummaryfun(i, mod1, mod2, mines_russ)

sink(paste0(i, "_allregions_summarystats.txt"), append=TRUE)
print(summary(mod3))
sink()

preds <- predict(mod3, newdata=mines_russ, se=TRUE)
preds <- bind_cols(mines_russ, data.frame(fit=preds$fit, se.fit=preds$se.fit))
p <-  preds %>%  
  ggplot(aes(x=year, y=znorm)) +
  geom_point(col="black") +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  geom_line(aes(y=fit), col="black") +
  theme(legend.position="none") +
  theme_minimal(24)
ggsave(filename=paste0(i, "_timeseries.png"), p, width = 9, height=7)

##########################
# MINING Plots for SI ----
##########################
#Plot trendlines for mines active over time (1950s to present) by region 
#OR for Russia and Finland vs rest of Arctic
#Model mining, aggregated except russia
#set up data ----
mines_agg <- mines_all %>% filter(Country!="Russia") %>%
              group_by(year) %>%
              summarise(nmines=sum(nmines, na.rm=TRUE)) %>% ungroup() %>% 
              mutate(Country="Rest of Arctic")

mod1 <- gamm(nmines ~ s(year), correlation=NULL, data=mines_agg, method="REML")

preds <- predict(mod1$gam, newdata=mines_agg, se=TRUE)
preds_all <- bind_cols(mines_agg, preds) 

#Model mines, russia 1950-2017
#Set up data
mines_russ <- mines_all %>% filter(Country=="Russia") 
#looks like we can do a lm
mod3 <- gamm(nmines ~ s(year), correlation=NULL, data=mines_russ, method="REML")

preds <- predict(mod3$gam, newdata=mines_russ, se=TRUE)
preds_russ <- bind_cols(mines_russ, preds) 

preds_all <- bind_rows(preds_all, preds_russ)

#Plot mining trends ----
p <- ggplot(preds_all, aes(x=year, y=nmines, fill=Country, group=Country)) +
  geom_area(stat='identity', position='identity', alpha=0.5) +
  #geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
  #geom_line(aes(y=fit, col=Country)) +
  coord_cartesian(xlim=c(1950, 2017), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("Number of mines in operation") +
  theme_minimal(18) +
  theme(legend.position="bottom") +
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
ggsave(filename=paste0("Arctic_Mining_Timeseries_bycountry_operational.png"), p, width=10, height=7)



require(tidyverse)
require(lubridate)
require(itsadug) #for gam visualisation
require(mgcv) #for gam mixed effects models
require(modelr)
require(gridExtra) #for grid.arrange
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

alldat <- read.csv(paste0(dirname(wd), "/Output/All_industries_long.csv"), fileEncoding = "UTF-8-BOM")

shipdist <- alldat %>% filter(Industry=="Shipping_distance") %>% droplevels() %>% 
              filter(year<2018) %>% #2018 estimates do not include full year
              group_by(Country, Region, subIndustry) %>%
              mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)
shiptraff <- alldat %>% filter(Industry=="Shipping_trafficwork") %>% droplevels() %>% 
              filter(year<2018) %>% #2018 estimates do not include full year
              group_by(Country, Region, subIndustry) %>%
              mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

##########################
### Set up plotting functions ----
##########################

modelsummaryfun <- function(i, m1, currdf, ...){
#modelsummaryfun <- function(i, m1, m2, currdf, ...){
  
  #plot the model autocorrelation and acf plot
  png(paste0(i, "_modelcomparison_ACFplots.png"), width=7, height=4.35, units="in", res=300)
  par(mfrow=c(1,2))
  acf(resid(m1), lag=5, main="ACF")
  pacf(resid(m1), lag=5, main="PACF")
  dev.off()
  
  sink(paste0(i, "_summarystats.txt"))
  print("model may have temporal autocorrelation")
  print(summary(m1))
  png(paste0(i, "_model_gamcheck.png"), width=7, height=7, units="in", res=300)
  par(mfrow=c(2,2))
  print(gam.check(m1))
  dev.off()
  sink()
  
  #Plot the model with and without temporal autocorrelation
  png(paste0(i, "_model_residuals.png"), width=7.77, height=4.35, units="in", res=200)
  par(mfrow=c(1,3))
  plot(m1)
  dev.off()
}

bestmodplot <- function(i, bestmod, currdf, region, shipcat){
    preds <- predict(bestmod, newdata=currdf, se=TRUE)
    preds <- bind_cols(currdf, preds)
    if(shipcat==TRUE){
    p <-  preds %>%  
      ggplot(aes(x=year, y=znorm)) +
      geom_point(col="black") +
      geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
      geom_line(aes(y=fit), col="black") +
      facet_wrap(~subIndustry) +
      theme(legend.position="none") +
      theme_minimal(14)
    ggsave(filename=paste0(i, "_timeseries_byshipcat.png"), p, width = 7.77, height=4.35)
    }
    if(region==TRUE){
    #Plot the model by region
    p <-  preds %>%  
      ggplot(aes(x=year, y=znorm)) +
      geom_point(col="black") +
      geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
      geom_line(aes(y=fit), col="black") +
      facet_wrap(~Region, nrow=3) +
      theme_minimal(18) +
      theme(legend.position="none", axis.text.x = element_text(size=10)) 
      
    ggsave(filename=paste0(i, "_timeseries_byregion.png"), p, width = 14, height=7)
    }
    
    p <-  preds %>%  
      ggplot(aes(x=year, y=znorm)) +
      geom_point(col="black") +
      geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
      geom_line(aes(y=fit), col="black") +
      theme(legend.position="none") +
      theme_minimal(18)
    ggsave(filename=paste0(i, "_timeseries_trend.png"), p, width = 7.77, height=4.35)
    
}


##########################
### Models ship distance----
##########################
### Models using 'GAM' function

i <- "Shipping_distance"

mod1 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re")+ s(subIndustry, bs="re"), data=shipdist, method='REML')
modelsummaryfun(i, mod1, shipdist)
bestmodplot(i, mod1, shipdist, TRUE, TRUE)

#Drop 2012
i <- "Shipping_distance_no2012"
shipdist_sub <- shipdist %>% filter(year>2012)

mod2 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re")+ s(subIndustry, bs="re"), data=shipdist_sub, method='REML')
modelsummaryfun(i, mod2, shipdist_sub)
bestmodplot(i, mod2, shipdist_sub, TRUE, TRUE)

# #Drop 2012, with only region as random effect
# i <- "Shipping_distance_no2012_randomregion"
# mod5 <- gam(znorm ~ s(year, k=1) + s(Region, bs="re"), data=shipdist_sub)
# bestmodplot(i, mod5, shipdist_sub)
# modelsummaryfun(i, mod5, shipdist_sub, TRUE, TRUE)
# 
# #Drop 2012, with only shipcat as random effect
# i <- "Shipping_distance_no2012_randomshipcat"
# mod6 <- gam(znorm ~ s(year, k=1) + s(subIndustry, bs="re"), data=shipdist_sub)
# bestmodplot(i, mod6, shipdist_sub)
# modelsummaryfun(i, mod6, shipdist_sub, TRUE, TRUE)

#aggregate ship cat, region as random effect
i <- "Shipping_distance_aggregatedshipcat_no2012"
shipdist_agg <- shipdist_sub  %>%
          group_by(Region, year) %>%
          summarise(value=sum(value)) %>%
          mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

mod7 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re"), data=shipdist_agg, method='REML')
bestmodplot(i, mod7, shipdist_agg, shipcat=FALSE, region=TRUE)
modelsummaryfun(i, mod7, shipdist_agg)

#aggregate region, shipcat as random effect
i <- "Shipping_distance_aggregatedregion_no2012"
shipdist_agg2 <- shipdist_sub  %>%
          group_by(subIndustry, year) %>%
          summarise(value=sum(value)) %>%
          mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

mod8 <- gam(znorm ~ s(year, k=4) + s(subIndustry, bs="re"), data=shipdist_agg2, method='REML')
bestmodplot(i, mod8, shipdist_agg2, region=FALSE, shipcat=TRUE)
modelsummaryfun(i, mod8, shipdist_agg2)


##########################
### Models ship trafficwork----
##########################
### Models using 'GAM' function
#value = distance sailed in nm * mean ship weight in gT

i <- "Shipping_trafficwork"

mod9 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re")+ s(subIndustry, bs="re"), data=shiptraff, method='REML')
modelsummaryfun(i, mod9, shiptraff)
bestmodplot(i, mod9, shiptraff, TRUE, TRUE)

#Drop 2012
i <- "Shipping_trafficwork_no2012"
shiptraff_sub <- shiptraff %>% filter(year>2012)

mod10 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re")+ s(subIndustry, bs="re"), data=shiptraff_sub, method='REML')
modelsummaryfun(i, mod10, shiptraff_sub)
bestmodplot(i, mod10, shiptraff_sub, TRUE, TRUE)


#aggregate ship cat, region as random effect
i <- "Shipping_trafficwork_aggregatedshipcat_no2012"
shiptraff_agg <- shiptraff_sub  %>%
  group_by(Region, year) %>%
  summarise(value=sum(value)) %>%
  mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

mod11 <- gam(znorm ~ s(year, k=4) + s(Region, bs="re"), data=shiptraff_agg, method='REML')
bestmodplot(i, mod11, shiptraff_agg, shipcat=FALSE, region=TRUE)
modelsummaryfun(i, mod11, shiptraff_agg)

#aggregate region, shipcat as random effect
i <- "Shipping_trafficwork_aggregatedregion_no2012"
shiptraff_agg2 <- shiptraff_sub  %>%
  group_by(subIndustry, year) %>%
  summarise(value=sum(value)) %>%
  mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

mod12 <- gam(znorm ~ s(year, k=4) + s(subIndustry, bs="re"), data=shiptraff_agg2, method='REML')
bestmodplot(i, mod12, shiptraff_agg2, region=FALSE, shipcat=TRUE)
modelsummaryfun(i, mod12, shiptraff_agg2)

##########################
### Plot all shipping for SI ----
##########################

preds1 <- predict(mod2, newdata=shipdist, se=TRUE) #no 2012
preds1 <- bind_cols(shipdist, preds1) %>% mutate(industry="Distance")
preds9 <- predict(mod10, newdata=shiptraff, se=TRUE) #no 2012
preds9 <- bind_cols(shiptraff, preds9) %>% mutate(industry="Trafficwork")
preds_all <- bind_rows(preds1, preds9) 

p <- ggplot(preds_all, aes(x=year, y=znorm, fill=industry, group=industry)) +
  #geom_area(stat='identity', position='identity', alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.5)+
  geom_line(aes(y=fit, col=industry)) +
  coord_cartesian(xlim=c(2013, 2017), ylim=c(-1.5, 1.5), expand=FALSE) + 
  #scale_x_continuous(breaks=seq(2000, 2016, 4)) +
  xlab("Year") + ylab("znorm") +
  theme_minimal(18) +
  theme(legend.position=c(0.7, 0.16)) +
  scale_color_manual(values = viridisLite::magma(5)[2:4], guide = "none") +
  scale_fill_manual(values = viridisLite::magma(5)[2:4],
                    name = element_blank(),
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(30 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5, label.hjust = 1, nrow = 1,
                      byrow = T, # also the guide needs to be reversed
                      reverse = T, label.position = "bottom"))

ggsave(filename=paste0("Arctic_Shipping_Timeseries_distanceandtrafficwork_no2012.png"), p, width=10, height=7)

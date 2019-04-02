require(tidyverse)
require(lubridate)
require(itsadug) #for gam visualisation
require(mgcv) #for gam mixed effects models
require(modelr)
require(gridExtra) #for grid.arrange
#require(ggfortify) #for autoplot
require(ggpubr) #for ggarrange

##########################
# Set directories ----
##########################

wd = "C:/Users/cru016/Documents/CONNECT/Paper_5_state_of_arctic/Analysis/Model"
#wd = "D:/Box Sync/Arctic/CONNECT/Paper_5_state_of_arctic/Analysis/Model"
setwd(wd)

alldat <- read.csv(paste0(dirname(wd), "/output/All_industries_long.csv"), fileEncoding = "UTF-8-BOM")

# Normalise data
znormdat <- alldat %>%  filter(!is.na(value)) %>% #drop years missing data
  filter(Industry=="Fishing") %>% droplevels() %>%
  group_by(Region) %>% 
  mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector) %>% ungroup()

##########################
### Set up plotting functions ----
##########################

modelsummaryfun <- function(i, m1, currdf, ...){
  #modelsummaryfun <- function(i, m1, m2, currdf, ...){
  
  #plot the model autocorrelation and acf plot
  png(paste0(i, "_modelcomparison_ACFplots.png"), width=7, height=4.35, units="in", res=300)
  par(mfrow=c(1,2))
  acf(resid(m1), lag=50, main="ACF")
  pacf(resid(m1), lag=50, main="PACF")
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

bestmodplot <- function(i, bestmod, currdf, region){
  preds <- predict(bestmod, newdata=currdf, se=TRUE)
  preds <- bind_cols(currdf, preds)
  if(region==TRUE){
    #Plot the model by region
    p <-  preds %>%  
      ggplot(aes(x=year, y=znorm)) +
      geom_point(col="black") +
      geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
      geom_line(aes(y=fit), col="black") +
      facet_wrap(~Region, ncol=4) +
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
  ggsave(filename=paste0(i, "_timeseries_trend.png"), p, width = 7.77, height=5)
  
}

##########################
### split into regions with similar trends ----
##########################
#I did this visually but ould use PCA
znormdat <- znormdat %>% mutate(Group = case_when(Region %in% c("Barents Sea", "Beaufort Sea", "Canadian Eastern Arctic -West Greenland", 
                                                                "Canadian High Arctic - North Greenland", "Hudson Bay Complex",
                                                                "Newfoundland-Labrador Shelf") ~ "Stock collapse",
                                                  Region %in% c("Aleutian Islands", "East Bering Sea", "East Siberian Sea", 
                                                                "Faroe Plateau", "Greenland Sea", "Iceland Shelf and Sea", 
                                                                "Kara Sea", "Laptev Sea", "Northern Bering - Chukchi Seas",  
                                                                "Norwegian Sea", "West Bering Sea") ~ "No collapse"))

##########################
### Model Fisheries ----
##########################
#Model all regions together ----
#This is a terrible model! (very low R2, gam.check looks bad)

#mod1 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation=NULL, data=znormdat, method="REML")
mod1 <- gam(znorm ~ s(year) + s(Region, bs="re"), data=znormdat, method="REML")

modelsummaryfun("Fishing_allregions", mod1, znormdat)
bestmodplot("Fishing_allregions", mod1, znormdat, TRUE)


#Model regions that had stock collapse ----
collapsedf <- znormdat %>% filter(Group=="Stock collapse") %>% droplevels

mod2 <- gam(znorm ~ s(year) + s(Region, bs="re"), data=collapsedf, method="REML")
modelsummaryfun2("Fishing_stockcollapseregions", mod2, collapsedf)
bestmodplot("Fishing_stockcollapseregions", mod2, collapsedf, TRUE)
bestmodplot("Fishing_stockcollapseregions", mod2, collapsedf, FALSE)

#Model regions that didn't have stock collapse ----

otherdf <- znormdat %>% filter(Group=="No collapse") %>% droplevels

mod3 <- gam(znorm ~ s(year) + s(Region, bs="re"), data=otherdf, method="REML")
modelsummaryfun2("Fishing_otherregions", mod3, otherdf)
bestmodplot("Fishing_otherregions", mod3, otherdf, TRUE)
bestmodplot("Fishing_otherregions", mod3, otherdf, FALSE)

##########################
### Plot all Fisheries ----
##########################

preds2 <- predict(mod2, newdata=collapsedf, se=TRUE) 
preds_all <- bind_cols(collapsedf, preds2)
preds3 <- predict(mod3, newdata=otherdf, se=TRUE) 
preds3 <- bind_cols(otherdf, preds3)
preds_all <- bind_rows(preds_all, preds3) 
preds_all$Group <- as.factor(preds_all$Group)


p <- ggplot(preds_all, aes(x=year, y=znorm, fill=Group, group=Group)) +
  #geom_point(col="grey70", alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.5)+
  geom_line(aes(y=fit, col=Group)) +
  coord_cartesian(xlim=c(1950, 2014), ylim=c(-2.5, 2.5), expand=FALSE) + 
  scale_x_continuous(breaks=seq(1960, 2010, 10)) +
  xlab("Year") + ylab("znorm") +
  theme_minimal(18) +
  theme(legend.position=c(0.7, 0.16)) +
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

ggsave(filename=paste0("Arctic_Fisheries_Timeseries.png"), p, width=10, height=7)


p <- ggplot(preds_all, aes(x=year, y=znorm, fill=Group, group=Group)) +
  #geom_point(col="grey70", alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.5)+
  geom_line(aes(y=fit, col=Group)) +
  coord_cartesian(xlim=c(2000, 2014), ylim=c(-2.5, 2.5), expand=FALSE) + 
  scale_x_continuous(breaks=seq(2000, 2012, 4)) +
  xlab("Year") + ylab("znorm") +
  theme_minimal(18) +
  theme(legend.position=c(0.7, 0.16)) +
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

ggsave(filename=paste0("Arctic_Fisheries_Timeseries_2000s.png"), p, width=10, height=7)



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

# Normalise data
znormdat <- alldat %>%  filter(!is.na(value)) %>% #drop years missing data
  filter(subIndustry %in% c("Cruise_tourism", "Domestic_tourism", "International_tourism")) %>%
  group_by(subIndustry, Region) %>% 
  mutate(znorm = scale(value, center=TRUE, scale=TRUE) %>% as.vector)

#how many values are there for each region?
n <- znormdat %>% 
  count()

#drop regions with insuffient data to calculate robust znorm
dropregions <- n[n$n <= 3, ] #drop with 3 or fewer observations
znormdat <- znormdat[!(znormdat$Region %in% dropregions$Region & znormdat$subIndustry %in% dropregions$subIndustry), ]

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

plotmodelbyregion <- function(i, bestmod, currdf, pwidth, pheight, fontscale){
  preds <- predict(bestmod$gam, newdata=currdf, se=TRUE)
  preds2 <- bind_cols(currdf, preds)
  
  p <- preds2 %>% ggplot(aes(x=year, y=znorm)) +
    geom_point(col="black") +
    geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.2, fill="grey30")+
    geom_line(aes(y=fit), col="black", lwd=0.7) +
    facet_wrap(~Region) +
    theme_light(18) +
    theme(legend.position="none", panel.grid=element_blank(), strip.text = element_text(colour = "black"))
  ggsave(filename=paste0(i, "_timeseries_bycountry.png"), p, width = pwidth, height=pheight, dpi=200, units="in", scale=fontscale)
}  

##########################
### Model Cruise tourism ----
##########################

i <- "Cruise_tourism"
cruisedf <- znormdat %>% filter(subIndustry==i) %>% ungroup() %>% droplevels()

mod1 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation=NULL, data=cruisedf, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod2 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation = corAR1(form = ~ year), data=cruisedf, method="REML")

modelsummaryfun(i, mod1, mod2, cruisedf)
plotmodelbyregion(i, mod1, cruisedf, 9, 7, 1)


##########################
### Model International tourism ----
##########################

i <- "International_tourism"
intdf <- znormdat %>% filter(subIndustry==i) %>% ungroup() %>% droplevels()

mod3 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation=NULL, data=intdf, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod4 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation = corAR1(form = ~ year), data=intdf, method="REML")

modelsummaryfun(i, mod3, mod4, intdf)
plotmodelbyregion(i, mod3, intdf, 12, 8, 1)


##########################
### Model Domestic tourism ----
##########################

i <- "Domestic_tourism"
domdf <- znormdat %>% filter(subIndustry==i) %>% ungroup() %>% droplevels()

mod5 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation=NULL, data=domdf, method="REML")
#Add in temporal autocorrelation with 1yr lag
mod6 <- gamm(znorm ~ s(year), random=list(Region=~1), correlation = corAR1(form = ~ year), data=domdf, method="REML")

modelsummaryfun(i, mod5, mod6, domdf)
plotmodelbyregion(i, mod5, domdf, 12, 8, 1)


##########################
### Plot all tourism ----
##########################

preds1 <- predict(mod1$gam, newdata=cruisedf, se=TRUE) 
preds1 <- bind_cols(cruisedf, preds1) %>% mutate(industry="Cruise")
preds3 <- predict(mod3$gam, newdata=intdf, se=TRUE) 
preds3 <- bind_cols(intdf, preds3) %>% mutate(industry="International")
preds_all <- bind_rows(preds1, preds3) 
preds5 <- predict(mod5$gam, newdata=domdf, se=TRUE) 
preds5 <- bind_cols(domdf, preds5) %>% mutate(industry="Domestic")
preds_all <- bind_rows(preds_all, preds5)


p <- ggplot(preds_all, aes(x=year, y=znorm, fill=industry, group=industry)) +
  #geom_area(stat='identity', position='identity', alpha=0.5) +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=0.5)+
  geom_line(aes(y=fit, col=industry)) +
  coord_cartesian(xlim=c(2000, 2017), ylim=c(-2.5, 2.5), expand=FALSE) + 
  scale_x_continuous(breaks=seq(2000, 2016, 4)) +
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

ggsave(filename=paste0("Arctic_Tourism_Timeseries_alltypes.png"), p, width=10, height=7)

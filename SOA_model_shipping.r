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

ship <- alldat %>% filter(Industry=="Shipping_distance") %>% droplevels()

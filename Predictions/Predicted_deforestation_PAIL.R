
##################################################
#### Deforestation predictions in 
## protected areas and indigenous lands
##################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(viridis)
library(gridExtra)
library(scales)
source("Deforestation_modutil_v4.R")

## Load PA and IL
PA <- st_read(dsn = "Shapefiles/PA", layer= "PA_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 161 features
IL <- st_read(dsn = "Shapefiles/IL", layer= "IL_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 393 features
#mem_used() # 578 MB

## Load predicted deforestation shapefiles
sh_16_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2022_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

sh_17_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2021_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

sh_18_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2020_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

## Intersect
Int_16_19_PA <- sf::st_intersection(sh_16_19_25, PA)
Int_17_19_PA <- sf::st_intersection(sh_17_19_25, PA)
Int_18_19_PA <- sf::st_intersection(sh_18_19_25, PA)

Int_16_19_IL <- sf::st_intersection(sh_16_19_25, IL)
Int_17_19_IL <- sf::st_intersection(sh_17_19_25, IL)
Int_18_19_IL <- sf::st_intersection(sh_18_19_25, IL)

plot(Int_16_19_PA[,1])
plot(Int_16_19_IL[,1])

def.2020 <- data.frame(Deforestation=c(sum(Int_18_19_PA$prd_df_), sum(Int_18_19_IL$prd_df_)),
                       Year = as.factor(2020), Area = c("PA", "IL"))
def.2021 <- data.frame(Deforestation=c(sum(Int_17_19_PA$prd_df_), sum(Int_17_19_IL$prd_df_)),
                       Year=as.factor(2021), Area = c("PA", "IL"))
def.2022 <- data.frame(Deforestation=c(sum(Int_16_19_PA$prd_df_),sum(Int_16_19_IL$prd_df_)),
                       Year=as.factor(2022), Area = c("PA", "IL"))

## Percentages
def.2022 %>% filter(Area=="PA") %>%
  .$Deforestation / sum(sh_16_19_25$prd_df_)

def.2022 %>% filter(Area=="IL") %>%
  .$Deforestation / sum(sh_16_19_25$prd_df_)

## Transform predicted values to million hectare
def.PAIL <- bind_rows(def.2020, def.2021, def.2022) %>% 
  mutate(Area = fct_relevel(Area, "PA")) %>%
  mutate(Deforestation = Deforestation/1000000) 

## Load deforestation data
## 2016-2019
LM_grd_16_19_25 <- read.csv("GridData/Temporal/LM_grd_16_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.PA.2016 <- LM_grd_16_19_25 %>%
  inner_join(Int_16_19_PA, by="grid_ID") %>%
  .$TotPix %>% sum()

sum.IL.2016 <- LM_grd_16_19_25 %>%
  inner_join(Int_16_19_IL, by="grid_ID") %>%
  .$TotPix %>% sum()

## 2017-2019
LM_grd_17_19_25 <- read.csv("GridData/Temporal/LM_grd_17_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.PA.2017 <- LM_grd_17_19_25 %>%
  inner_join(Int_17_19_PA, by="grid_ID") %>%
  .$TotPix %>% sum()

sum.IL.2017 <- LM_grd_17_19_25 %>%
  inner_join(Int_17_19_IL, by="grid_ID") %>%
  .$TotPix %>% sum()

## 2018-2019
LM_grd_18_19_25 <- read.csv("GridData/Temporal/LM_grd_18_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.PA.2018 <- LM_grd_18_19_25 %>%
  inner_join(Int_18_19_PA, by="grid_ID") %>%
  .$TotPix %>% sum()

sum.IL.2018 <- LM_grd_18_19_25 %>%
  inner_join(Int_18_19_IL, by="grid_ID") %>%
  .$TotPix %>% sum()

## RMSE
list.files("Cluster/2019_Data/Full_20var/grd_16_19_25", pattern="Excluded")
INLA.RES.16_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_16_19_25/INLA.RES_7.csv", head=TRUE)

list.files("Cluster/2019_Data/Full_20var/grd_17_19_25", pattern="Excluded")
INLA.RES.17_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_17_19_25/INLA.RES_11.csv", head=TRUE)

list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")
INLA.RES.18_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_18_19_25/INLA.RES_9.csv", head=TRUE)

## Merge rmse
def.PAIL$RMSE.ha <- c((INLA.RES.18_19[1,2] * sum.PA.2018 * 0.09),
                      (INLA.RES.18_19[1,2] * sum.IL.2018 * 0.09),
                      (INLA.RES.17_19[1,2] * sum.PA.2017 * 0.09),
                      (INLA.RES.17_19[1,2] * sum.IL.2017 * 0.09),
                      (INLA.RES.16_19[1,2] * sum.PA.2016 * 0.09),
                      (INLA.RES.16_19[1,2] * sum.IL.2016 * 0.09))

def.PAIL <- def.PAIL %>%  mutate(RMSE.ha.million = RMSE.ha/1000000)

#### Plot
plot.PAIL <- def.PAIL %>% ggplot(aes(x=Year, y=Deforestation, fill=Area)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  geom_errorbar(aes(ymin=Deforestation, ymax=Deforestation+RMSE.ha.million), 
                width=.4, position=position_dodge(.9)) +
  scale_fill_brewer(palette="Paired", breaks=c("PA", "IL"),
                    labels=c("Protected Areas", "Indigenous lands")) +
  ylab("Predicted deforestation (million hectare)") +
  xlab("Year") +
  theme_bw() + theme(
    legend.position = "top", 
    legend.title=element_text(size=10, face="bold"),
    axis.text.x = element_text(size=10), 
    axis.title.x = element_text(size=15, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

plot.PAIL
ggsave("Results/PredDef.PAIL.png", plot = plot.PAIL, device = "png", width = 15, height = 15, units = "cm", dpi = 300)


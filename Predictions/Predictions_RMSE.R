##################################################
#### Deforestation yearly predictions and errors
##################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
source("Deforestation_modutil_v4.R")

## Load predicted deforestation shapefiles
sh_16_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2022_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

sh_17_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2021_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

sh_18_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2020_Predictions") %>%
  sf::st_transform(st_crs(4326)) 

## Sum total predicted deforestation per year
def.2022 <- data.frame(Deforestation=sum(sh_16_19_25$prd_df_), Year=as.factor(2022))
def.2021 <- data.frame(Deforestation=sum(sh_17_19_25$prd_df_), Year=as.factor(2021))
def.2020 <- data.frame(Deforestation=sum(sh_18_19_25$prd_df_), Year=as.factor(2020))

## Transform predicted values to million hectare
def.TOT <- bind_rows(def.2020, def.2021, def.2022) %>% 
  mutate(Deforestation = Deforestation/1000000) 

## Load deforestation data
## 2016-2019
LM_grd_16_19_25 <- read.csv("GridData/Temporal/LM_grd_16_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.2016 <- LM_grd_16_19_25 %>%
  inner_join(sh_16_19_25, by="grid_ID") %>%
  .$TotPix %>% sum()

## 2017-2019
LM_grd_17_19_25 <- read.csv("GridData/Temporal/LM_grd_17_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.2017 <- LM_grd_17_19_25 %>%
  inner_join(sh_17_19_25, by="grid_ID") %>%
  .$TotPix %>% sum()

## 2018-2019
LM_grd_18_19_25 <- read.csv("GridData/Temporal/LM_grd_18_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid_ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid_ID = as.factor(grid_ID)) %>%
  dplyr::select(c(grid_ID, TotPix)) 

sum.2018 <- LM_grd_18_19_25 %>%
  inner_join(sh_18_19_25, by="grid_ID") %>%
  .$TotPix %>% sum()

## RMSE
list.files("Cluster/2019_Data/Full_20var/grd_16_19_25", pattern="Excluded")
INLA.RES.16_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_16_19_25/INLA.RES_7.csv", head=TRUE)

list.files("Cluster/2019_Data/Full_20var/grd_17_19_25", pattern="Excluded")
INLA.RES.17_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_17_19_25/INLA.RES_11.csv", head=TRUE)

list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")
INLA.RES.18_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_18_19_25/INLA.RES_9.csv", head=TRUE)

## Merge rmse
def.TOT$RMSE.ha <- c((INLA.RES.18_19[1,2] * sum.2018 * 0.09),
                      (INLA.RES.17_19[1,2] * sum.2017 * 0.09),
                      (INLA.RES.16_19[1,2] * sum.2016 * 0.09))

def.TOT <- def.TOT %>%  mutate(RMSE.ha.million = RMSE.ha/1000000)
def.TOT

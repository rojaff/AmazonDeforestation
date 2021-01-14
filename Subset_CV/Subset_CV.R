
################################################################
#### Full model for the ALtamira region
##  containing all 20 predictor variables 
##                    &                   
##      Spatial blocks cross validation
################################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(gstat)
library(tictoc)
library(INLA)
source("Deforestation_modutil_v4.R")

## Load 5x5km 2018-2019 deforestation data for Altamira
past.def.inla <- read.csv("Subset_CV/LanMetricsLoop_Altamira_18_19_5.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_3, pland_33, pland_12, pland_41, pland_24,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB)) %>%
  mutate(pland_15 = replace_na(pland_15, 0))

## Scale predictor variables
past.def.inla[, c(7:ncol(past.def.inla))] <- scale(past.def.inla[, c(7:ncol(past.def.inla))])

## Check data
nrow(past.def.inla) 
head(past.def.inla) 

###### Benchmarking spatial k-fold cross-validation ----
## Check Mesh - DO NOT USE max.edge < 0.03!!
(m1 <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(0.05, 1), cutoff=0.05, offset=c(-0.2,-0.2)))$n
#(m1 <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(0.03, 1), cutoff=0.035, offset=c(-0.2,-0.2)))$n
plot(m2, asp=1) 
points(past.def.inla[, c("Long", "Lat")], col="red", pch=19)

## Full formula 
hyper.prec <- list(theta = list(prior="pc.prec", param = c(1, 0.05)))

f1 <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE) + 
  pland_15 +
  len.st.roads + len.fe.roads + dist.roads +
  len.rivers + len.waterways + dist.rivers + 
  area.protected + area.indigenous + 
  IDHM + POP_2018 + PIB_PC +     
  dem.data + slope.data + wi.data + 
  dist.mining +  dist.urban + dist.pa + dist.il + 
  f(field, model=spde) + f(grid.ID, model='iid') ## w accounts for SA, grid.ID accounts for OD

rmse <- inla.spatial.cv(data=past.def.inla, formula=f1, mesh=m1, k=5)
# 2314 rows: 13 mins | 441 rows: 5.941403 mins | 143 rows: 10.90336
# smaller areas (clip3) increase computing time! (maybe higher SA)

################ Step-wise model selection using spatial cross-validation ----
varnames <- c("f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE)", 
              "pland_15", 
              "len.st.roads", "len.fe.roads", "dist.roads", 
              "len.rivers", "len.waterways", "dist.rivers", 
              "area.protected", "area.indigenous", 
              "IDHM", "POP_2018", "PIB_PC",     
              "dem.data", "slope.data", "wi.data", 
              "dist.mining",  "dist.urban", "dist.pa", "dist.il") 

tic()
inla.spatial.step(varnames) 
toc() # LanMetricsLoop_Altamira_18_19_5.csv

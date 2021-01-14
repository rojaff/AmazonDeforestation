
##############################################################
#### Assessment of model accuracy across spatial scales  ####
##############################################################

#### NOTE
## Cross validation for three years (16, 17 & 18) excluded the following variables:
## dist.mining, dist.urban, len.st.roads, pland_15, wi.data
###################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
library(lares) # devtools::install_github("laresbernardo/lares")
library(lme4)
library(sp)
library(rgdal)
library(raster)
library(gstat)
library(tictoc)
library(INLA)
source("Deforestation_modutil_v4.R")

#################### Load and prepare data ----
## Function to load data
read_scale_data <- function(data){
  scale_data <- read.csv(data, head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                     pland_0, pland_12, pland_23, pland_32, pland_33,
                     pland_5, pland_39, pland_4, pland_41, pland_25,
                     pland_9, pland_24, pland_30, pland_20, pland_11,
                     CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB, 
                     loss.1, parent_id.1, no_change.1, 
                     pland_3, pland_15, 
                     dist.mining, dist.urban, len.st.roads, wi.data)) %>%
  na.omit() #%>% filter(Def>0) 
  
  ## Scale predictor variables
  scale_data[, c(7:ncol(scale_data))] <- scale(scale_data[, c(7:ncol(scale_data))])
  return(scale_data)
}

## Load data
list.files("Scale_test/")

inla_s_5 <- read_scale_data("GridData/Spatial/LM_grd_18_19_5.csv")
inla_s_10 <- read_scale_data("GridData/Spatial/LM_grd_18_19_10.csv")
inla_s_15 <- read_scale_data("GridData/Spatial/LM_grd_18_19_15.csv")
inla_s_20 <- read_scale_data("GridData/Spatial/LM_grd_18_19_20.csv")
inla_s_25 <- read_scale_data("GridData/Spatial/LM_grd_18_19_25.csv")
inla_s_30 <- read_scale_data("GridData/Spatial/LM_grd_18_19_30.csv")
inla_s_35 <- read_scale_data("GridData/Spatial/LM_grd_18_19_35.csv")
inla_s_40 <- read_scale_data("GridData/Spatial/LM_grd_18_19_40.csv")

head(inla_s_40)
nrow(inla_s_40)

## Check correlations
cor <- inla_s_5 %>% dplyr::select(-c(grid.ID, Long, Lat, Def, NDef, TotPix))

corr_cross(cor, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10) # display top 10 couples of variables (by correlation coefficient)

corr_var(cor, # name of dataset
         ed_3, # name of variable to focus on
         top = 5) # display top 5 correlations

#################### Spatial k-fold cross-validation----
## Formula
hyper.prec <- list(theta = list(prior="pc.prec", param = c(1, 0.05)))

# Excluded in CV: dist.mining, dist.urban, len.st.roads, pland_15, wi.data
f1 <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE) + 
  len.fe.roads + dist.roads +
  len.rivers + len.waterways + dist.rivers + 
  area.protected + area.indigenous + 
  IDHM + POP_2018 + PIB_PC +     
  dem.data + slope.data + 
  dist.pa + dist.il + 
  f(field, model=spde) + f(grid.ID, model='iid') ## w accounts for SA, grid.ID accounts for OD

## Mesh 
(m8 <- inla.mesh.2d(loc = inla_s_40[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m7 <- inla.mesh.2d(loc = inla_s_35[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m6 <- inla.mesh.2d(loc = inla_s_30[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m5 <- inla.mesh.2d(loc = inla_s_25[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m4 <- inla.mesh.2d(loc = inla_s_20[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m3 <- inla.mesh.2d(loc = inla_s_15[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m2 <- inla.mesh.2d(loc = inla_s_10[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
(m1 <- inla.mesh.2d(loc = inla_s_5[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
plot(m8, asp=1) 
points(inla_s_10[, c("Long", "Lat")], col="red", pch=19)

## Spatial k-fold cross-validation
#rmse_40 <- inla.spatial.cv(data=inla_s_40, formula=f1, mesh=m1, k=5) # 11 min
#rmse_35 <- inla.spatial.cv(data=inla_s_35, formula=f1, mesh=m1, k=5)
#rmse_30 <- inla.spatial.cv(data=inla_s_30, formula=f1, mesh=m1, k=5)
#rmse_25 <- inla.spatial.cv(data=inla_s_25, formula=f1, mesh=m1, k=5)
#rmse_20 <- inla.spatial.cv(data=inla_s_20, formula=f1, mesh=m1, k=5)
#rmse_15 <- inla.spatial.cv(data=inla_s_15, formula=f1, mesh=m1, k=5)
#rmse_10 <- inla.spatial.cv(data=inla_s_10, formula=f1, mesh=m1, k=5) # 1.228062 hours
#rmse_5 <- inla.spatial.cv(data=inla_s_5, formula=f1, mesh=m1, k=5) # 6.276363 hours (in cluster)

#rmse.results <- data.frame(rmse_5, rmse_10, rmse_15, rmse_20, rmse_25, rmse_30, rmse_35, rmse_40)
#rmse.table <- pivot_longer(rmse.results, cols = rmse_5:rmse_40)
#write.csv(rmse.table, file="Scale_results.csv", row.names=FALSE)
rmse.table <- read.csv("Results/Scale_results.csv", head=TRUE)
summary(rmse.table$value)

sp <- rmse.table %>% mutate(scale = c(5, 10, 15, 20, 25, 30, 35, 40)) %>%
  ggplot(aes(x=scale, y=value)) + geom_point(size=2) + 
  geom_line(size=2, alpha=0.75) +
  xlab("Grid size (km)") +
  ylab(NULL) + 
  ylim(0.010, 0.028) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))

sp
ggsave(filename = "Results/scale_test.png", plot = sp, 
       device = "png",
       width = 15,
       height = 15,
       units = "cm",
       dpi = 150)



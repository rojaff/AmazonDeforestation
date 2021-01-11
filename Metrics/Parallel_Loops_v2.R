
##############################################################
#### Parallelization of grid-level metrics  ####
##############################################################

## Clean workspace
rm(list=ls())
gc()

## Load libraries
library(tictoc)
library(doParallel)
library(foreach)
library(plyr)
library(pryr) #mem_used()
library(lwgeom)
library(sf)
source("Metrics/LandscapeMetrics.R")

######### Load data ----
## Load GEE Deforestation grids
list.files("Shapefiles/GEE/2019_fullgrids", pattern=".shp")

## All scales - 18_19 
#grd_18_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_5_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) #%>% nrow()
#grd_18_19_10 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_10_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_18_19_15 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_15_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_18_19_20 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_20_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
grd_18_19_25 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_18_19_30 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_30_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_18_19_35 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_35_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_18_19_40 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_40_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 

## 25km scale - All years (Full CV)
#grd_18_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_25_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_17_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2017_2019_25_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_16_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2016_2019_25_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_15_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2015_2019_25_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 

## 5km scale - All years
#grd_16_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2016_2019_5_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
#grd_17_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2017_2019_5_7") %>%
#  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
grd_18_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_5_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 

## Reference grids (including no loss grids)
#grd_18_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_25_7") %>%
#  sf::st_transform(st_crs(4326)) 
grd_18_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_5_7") %>%
  sf::st_transform(st_crs(4326))

nrow(grd_18_19_5)
nrow(grd_18_19_40)
#mem_used() 

## Load Altamira_clip2 shapefile
Altamira <- st_read(dsn = "Shapefiles/Altamira_clip", layer= "Altamira_clip2") %>%
  sf::st_transform(st_crs(4326))
# sqrt(st_area(Altamira))

## Intercept Altamira_clip2 shapefile and grid
Altamira_int_16 <- sf::st_intersection(Altamira, grd_16_19_5) 
Altamira_int_17 <- sf::st_intersection(Altamira, grd_17_19_5) 
Altamira_int_18 <- sf::st_intersection(Altamira, grd_18_19_5) 
Altamira_bbox <- st_as_sfc(st_bbox(Altamira_int_18))
#st_write(st_as_sfc(st_bbox(Altamira_int_18)), "Shapefiles/Altamira_clip/Altamira_bbox.shp")

#plot(Altamira_int_18[, 2])

## Load MapBiomas rasters
#amazonia <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2019.tif") 
amazonia <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2018.tif") 
#amazonia <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2017.tif") 
#amazonia <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2016.tif") 
#amazonia <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2015.tif") 
#mem_used() # 117 MB

## Load roads
re <- st_read(dsn = "Shapefiles/Roads", layer= "R_estaduais_mapbiomas") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buf fer(dist = 0) # 45120 features
rf <- st_read(dsn = "Shapefiles/Roads", layer= "R_federais_dnit") %>%
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 2137 features
rall <- st_read(dsn = "Shapefiles/Roads", layer= "R_all") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 209019 features
roadsL <- list(re, rf, rall) # 243 MB
#object_size(roadsL)
#mem_used() # 373 MB

## Load rivers
RIV <- st_read(dsn = "Shapefiles/Rivers", layer= "IBGE_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 891012 features
WAT <- st_read(dsn = "Shapefiles/Rivers", layer= "MapBiomas_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 525 features
RIV.WAT <- st_read(dsn = "Shapefiles/Rivers", layer= "Rivers_all") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) #  891537 feature
riversL <- list(RIV, WAT, RIV.WAT) # 796 MB *** Heaviest object!
#object_size(riversL)
#mem_used() # 543 MB

## Load PA and IL
PA <- st_read(dsn = "Shapefiles/PA", layer= "PA_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 161 features
IL <- st_read(dsn = "Shapefiles/IL", layer= "IL_AL") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) # 393 features
PAIL <- list(PA, IL)
#mem_used() # 578 MB

## Load social municipality variables
SOC <- st_read(dsn = "Shapefiles/Social_var_mun", layer="Social_var_mun") %>% 
  sf::st_transform(st_crs(4326)) #%>%  sf::st_buffer(dist = 0) 
#mem_used() # 603 MB

## Load abiotic rasters 
INPE.dem <- raster("INPE/DEM_inpe_AL-fix.tif")
INPE.slope <- raster("INPE/Slope_inpe_AL-fix.tif")
INPE.wi <- raster("INPE/WI_inpe_AL-fix.tif")
INPE <- list(INPE.dem, INPE.slope, INPE.wi)

##### Load Mining and Urban shapefiles
## Change year
mining <- st_read(dsn = "Shapefiles/Mining", layer= "mining_2018_AL") %>% sf::st_transform(st_crs(4326))
urban <- st_read(dsn = "Shapefiles/Urban", layer= "urban_2018_AL") %>% sf::st_transform(st_crs(4326))
MUL <- list(mining, urban)

########## Parallel Loop -----
detectCores()

tic()
cl  <- makeCluster(2) # makeCluster(spec=24, type='SOCK')
registerDoParallel(cl)

## Change i ids
RES <- foreach(i=1:nrow(Altamira_int_18[1:2,]), .combine='rbind.fill', .multicombine=TRUE, #.export=c("grd", "rd.tr")
               .packages=c('raster', 'sp', 'rgdal', 'landscapemetrics', 'plyr', 'sf', 'lwgeom')) %dopar% {
                 source("LandscapeMetrics.R", local=TRUE)
                 LanMetrics(raster=amazonia_2018, grid=Altamira_int_18[1:2,], grid.index=i, 
                            roads.list=roadsL, rivers.list=riversL, pail.list=PAIL,
                            soc.shapefile=SOC, abiotic.list=INPE, mining_urban.list=MUL) 
               }
stopCluster(cl)
toc()
#RF <- RES[rowSums(RES, na.rm = TRUE)!=0, ]
write.csv(RES, file = "LM_grd_18_19_40.csv", row.names=FALSE)

######## Function to run parallel loop ----
LanMetricsLoop <- function(ncores=2, def.grid, name){
  cl  <- makeCluster(ncores) 
  registerDoParallel(cl)

  RES <- foreach(i=1:nrow(def.grid), .combine='rbind.fill', .multicombine=TRUE, 
                .export=c("amazonia", "roadsL", "riversL", "PAIL", "SOC", "INPE", "MUL"),
                .packages=c('raster', 'sp', 'rgdal', 'landscapemetrics', 'plyr', 'sf', 'lwgeom')) %dopar% {
                 source("LandscapeMetrics.R", local=TRUE)
                 LanMetrics(raster=amazonia, grid=def.grid, grid.index=i, 
                            roads.list=roadsL, rivers.list=riversL, pail.list=PAIL,
                            soc.shapefile=SOC, abiotic.list=INPE, mining_urban.list=MUL) 
               }
  stopCluster(cl)
  write.csv(RES, file = name, row.names=FALSE)
  }

LanMetricsLoop(ncores=4, def.grid=Altamira_int_18, name="LanMetricsLoop_Altamira_18_19_5.csv")

#### Times in cluster:
# 60 cores - grd_17_18_1: 9h 
# 80 cores - grd_17_18_5: 6h 
# 80 cores - grd_17_18_10: 6.8h

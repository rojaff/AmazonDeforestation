
##########################################################
#### Set of functions to compute grid-level metrics  ####
##########################################################

## Load libraries
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(lwgeom)
library(sf)
library(landscapemetrics)

########## degree2distance -----
## Function to transform decimal degrees to km 
degree2distance <- function(degrees){
  res <- degrees*(111.32) ## at equator 1 decimal degree = 111.132 km
  print(paste(res, "km"))
}

########### val.check ----
## Check shapefiles for validity
val.check <- function(SH){
  reVAL <- st_is_valid(SH)
  RES <- reVAL[reVAL==FALSE]
  return(RES)
}

########### RR.length -----
## Function to calculate total line length inside grid
RR.length <- function(SH, grid){
  int_line <- sf::st_intersection(SH, grid) # Find the intersections
  #print(paste(nrow(int_line), "lines"))
  if(nrow(int_line)==0){ # Set zero area when no lines are present
    RES <- 0
  }else{
    RES <- as.numeric(sum(sf::st_length(int_line)))/1000 # Calculate total length of lines (km)
  }
  return(RES)
}

############## RR.dist----
## Function to calculate distance to nearest line
RR.dist <- function(centroid, SH){
  dist.n.r <- st_distance(centroid, SH)
  RES <- min(dist.n.r)/1000 # km
  return(as.numeric(RES))
}

############## PAIL.area ----
## Function to calculate total polygon area inside grid
PAIL.area <- function(SH, grid){
  int_poly <- sf::st_intersection(SH, grid) # Find the intersections
  #print(paste(nrow(int_line), "polygons"))
  if(nrow(int_poly)==0){ # Set zero area when no polygons are present
    RES <- 0
  }else{
    RES <- as.numeric(sum(sf::st_area(int_poly)))/1000000 # Calculate area of polygon (km2)
  }
  return(RES)
}

########## SOC.mu ----
## Function to intercept municipality polygon and add NAs if need
SOC.mu <- function(SH, centroid){
  SOC_int <- st_intersection(SH, centroid)
  st_geometry(SOC_int) <- NULL
  if(nrow(SOC_int)==0){
    SOC_int[1,1:9] <- NA
    return(SOC_int)
  } else{
    return(SOC_int[1,1:9])
  }
}

#### LanMetrics  ---- 
## Function to calculate all grid-level metrics
LanMetrics <- function(raster, grid, grid.index, 
                       roads.list, rivers.list, pail.list,
                       soc.shapefile, abiotic.list, mining_urban.list){ 
  gc()
  
  ## Select one grid
  grd.red <- grid[grid.index, ]
  
  ## Skip cases where grid does not intersect raster
  int <- st_intersection(st_as_sfc(st_bbox(raster)), st_as_sfc(st_bbox(grd.red)))
  if(length(int)==0){
    RES <- data.frame(parent_id=NA)
    return(RES)
  } else {
    
  ## Skip empty rasters
  DEF <- grd.red %>% dplyr::select(parent_id, loss, no_change) %>%
    dplyr::mutate(tot_pixels = loss + no_change) %>% st_set_geometry(NULL)
  
  if(DEF$tot_pixels==0){
    RES <- data.frame(parent_id=NA) 
    return(RES)
  } else {
    
  ## Crop raster to grid
  rasterC <- crop(raster, as(grd.red, "Spatial")) 
  
  ## Get coordinates for grid centroid
  suppressWarnings(cen <- st_centroid(grd.red))
      
  ###### Landscape metrics on t1 rasters
  p.class <- lsm_c_pland(rasterC) ## Percentage of landscape of class 
      
  div <- lsm_c_division(rasterC, directions = 8) %>% ## Division index
    dplyr::filter(class==3)
      
  e.density <- lsm_c_ed(rasterC) %>% ## Edge density
    dplyr::filter(class==3)
      
  all_lm <- bind_rows(p.class, e.density, div) %>%
    dplyr::mutate(name=paste(metric, "_", class, sep="")) %>%
    dplyr::select(name, value) %>%
    tidyr::spread(name, value) 
      
   rm(rasterC)
      
  ## Roads
  len.re <- RR.length(roads.list[[1]], grd.red)
  len.rf <- RR.length(roads.list[[2]], grd.red)
  dist.ref <- RR.dist(cen, roads.list[[3]]) # Use all roads
      
  ## Rivers
  len.riv <- RR.length(rivers.list[[1]], grd.red)
  len.wat <- RR.length(rivers.list[[2]], grd.red)
  dist.riv.wat <- RR.dist(cen, rivers.list[[3]]) # Use all 
      
  ## Protected areas (PA) & Indigenous lands (IL)
  area.pa <- PAIL.area(pail.list[[1]], grd.red)
  area.il <- PAIL.area(pail.list[[2]], grd.red)
      
  ## Social variables at municipality scale
  SOC_RES <- SOC.mu(soc.shapefile, cen)
      
  ## INPE data
  dem <- raster::extract(abiotic.list[[1]], st_coordinates(cen), buffer=2500, fun=median)
  slope <- raster::extract(abiotic.list[[2]], st_coordinates(cen), buffer=2500, fun=median)
  wi <- raster::extract(abiotic.list[[3]], st_coordinates(cen), buffer=2500, fun=median)
      
  ## Distances
  DMining <- RR.dist(cen, mining_urban.list[[1]])
  DUrban <- RR.dist(cen, mining_urban.list[[2]])
  DPA <- RR.dist(cen, pail.list[[1]])
  DIL <- RR.dist(cen, pail.list[[2]])
      
  ## Create final dataframe and return result
  RES <- data.frame(DEF, 
                    Long=st_coordinates(cen)[1], Lat=st_coordinates(cen)[2], 
                    all_lm,
                    len.st.roads=len.re, len.fe.roads=len.rf, dist.roads=dist.ref,
                    len.rivers=len.riv, len.waterways=len.wat, dist.rivers=dist.riv.wat,
                    area.protected=area.pa, area.indigenous=area.il,
                    SOC_RES, dem.data=dem, slope.data=slope, wi.data=wi,
                    dist.mining=DMining, dist.urban=DUrban, dist.pa=DPA, dist.il=DIL)
      
   return(RES)
    }
  }
}


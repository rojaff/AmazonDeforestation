
################################################################
#### Deforestation predictions based on the selected models ####
################################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
#library(lares) # devtools::install_github("laresbernardo/lares")
library(sp)
library(sf)
library(rgdal)
library(raster)
library(tictoc)
library(INLA)
source("Deforestation_modutil_v4.R")

#### Load Deforestation dataset ----
## Load data, select variables and replace pland NAs with zeros
## 2016-2019
past.def.inla <- read.csv("GridData/Temporal/LM_grd_16_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_0, pland_12, pland_23, pland_32, pland_33,
                   pland_5, pland_39, pland_4, pland_41, pland_25,
                   pland_9, pland_24, pland_30, pland_20, pland_11,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB, 
                   loss.1, parent_id.1, no_change.1)) %>%
  mutate(pland_3 = replace_na(pland_3, 0)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit() #%>% filter(Def>0) 

## 2017-2019
past.def.inla <- read.csv("GridData/Temporal/LM_grd_17_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_0, pland_12, pland_23, pland_32, pland_33,
                   pland_5, pland_39, pland_4, pland_41, pland_25,
                   pland_9, pland_24, pland_30, pland_20, pland_11,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB, 
                   loss.1, parent_id.1, no_change.1)) %>%
  mutate(pland_3 = replace_na(pland_3, 0)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit() #%>% filter(Def>0) 

## 2018-2019
past.def.inla <- read.csv("GridData/Temporal/LM_grd_18_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_0, pland_12, pland_23, pland_32, pland_33,
                   pland_5, pland_39, pland_4, pland_41, pland_25,
                   pland_9, pland_24, pland_30, pland_20, pland_11,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB, 
                   loss.1, parent_id.1, no_change.1)) %>%
  mutate(pland_3 = replace_na(pland_3, 0)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit() #%>% filter(Def>0) 

## Scale predictor variables
past.def.inla[, c(7:ncol(past.def.inla))] <- scale(past.def.inla[, c(7:ncol(past.def.inla))])

## Check data
nrow(past.def.inla) 
head(past.def.inla) 

#### Load reference 2019 data ----
lm.last <- read.csv("GridData/Forest/LM_grd_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_0, pland_12, pland_23, pland_32, pland_33,
                   pland_5, pland_39, pland_4, pland_41, pland_25,
                   pland_9, pland_24, pland_30, pland_20, pland_11,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB, 
                   loss.1, parent_id.1, no_change.1)) %>%
  mutate(pland_3 = replace_na(pland_3, 0)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit() %>% 
  semi_join(past.def.inla, by = "grid.ID") ##Filter deforested grids for selected period

## Scale predictor variables
lm.last[, c(7:ncol(lm.last))] <- scale(lm.last[, c(7:ncol(lm.last))])

## Check data
nrow(lm.last) 
head(lm.last) 

######## Predict reference data
# https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplespatial.html
mesh <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2))

## Formulas - After cross-validation
hyper.prec <- list(theta = list(prior="pc.prec", param = c(1, 0.05)))

list.files("Cluster/2019_Data/Full_20var/grd_16_19_25", pattern="Excluded")
best.formula(dir="Cluster/2019_Data/Full_20var/grd_16_19_25/", 
             res="INLA.RES_7", form="formulas_7")

# formula <- y ~ -1 + Intercept + 
#  f(inla.group(ed_3), model = "rw2", hyper = hyper.prec, scale.model = TRUE) + 
#  len.fe.roads + dist.roads + len.rivers + 
#  len.waterways + dist.rivers + area.protected + area.indigenous + 
#  POP_2018 + PIB_PC + dem.data + slope.data + dist.pa + dist.il + 
#  f(field, model = spde) + f(grid.ID, model = "iid")

list.files("Cluster/2019_Data/Full_20var/grd_17_19_25", pattern="Excluded")
best.formula(dir="Cluster/2019_Data/Full_20var/grd_17_19_25/", 
             res="INLA.RES_11", form="formulas_11")
# formula <- y ~ -1 + Intercept + 
# f(inla.group(ed_3), model = "rw2", hyper = hyper.prec, scale.model = TRUE) + 
# dist.roads + len.rivers + len.waterways + 
#  dist.rivers + area.protected + area.indigenous + PIB_PC + 
#  slope.data + dist.pa + 
# f(field, model = spde) + f(grid.ID, model = "iid")

list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")
best.formula(dir="Cluster/2019_Data/Full_20var/grd_18_19_25/", 
             res="INLA.RES_9", form="formulas_9")
 formula <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model = "rw2", hyper = hyper.prec, scale.model = TRUE) + 
  len.fe.roads + dist.roads + len.rivers + 
  len.waterways + area.protected + area.indigenous + IDHM + 
  PIB_PC + dem.data + slope.data + dist.il + 
  f(field, model = spde) + f(grid.ID, model = "iid")

# Define penalised complexity priors for random field. 
spde <- INLA::inla.spde2.matern(mesh, alpha=2)
field.indices <- inla.spde.make.index("field", n.spde = mesh$n)

# Make the A matrices
Aest <- INLA::inla.spde.make.A(mesh, loc = as.matrix(past.def.inla[, c("Long", "Lat")]))
Apred <- INLA::inla.spde.make.A(mesh, loc = as.matrix(lm.last[, c("Long", "Lat")]))

# Stack for estimation
stk.est <- inla.stack(tag = "estimation", ## tag
                      data = list(y = past.def.inla$Def, ## Number of deforested pixels
                                  Ntrials = past.def.inla$TotPix), ## Total number of pixels per grid
                      A = list(Aest, 1),  ## Projector matrix for space, fixed.
                      effects = list(field = field.indices,
                                     cbind(Intercept = 1, past.def.inla)))

# Stack for prediction
stk.pred <- inla.stack(tag = "prediction", ## tag
                      data = list(y = NA, ## Number of deforested pixels
                                  Ntrials = NA), ## Total number of pixels per grid
                      A = list(Apred, 1),  ## Projector matrix for space, fixed.
                      effects = list(field = field.indices,
                                         cbind(Intercept = 1, lm.last)))

# Full stack
stk.full <- inla.stack(stk.est, stk.pred)
    
# Run the model
tic()
m <- inla(formula, family = "binomial", 
              data = INLA::inla.stack.data(stk.full), 
              Ntrials = INLA::inla.stack.data(stk.full)$Ntrials, # can also simply use Ntrials
              #control.family = list(link = "logit"), # does not change predictions
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
              #control.inla = list(int.strategy = "eb", strategy = "gaussian")) # This makes things quicker. Might be a good idea for cv and then do the most accurate you can for the final model
toc() # default: 21.6 min, 30min, 26min
save(m, file="inla_18_19_25_FullCV.RData")
          
## Load models
inla_16_19 <- loadRData("inla_16_19_25_FullCV.RData")
inla_17_19 <- loadRData("inla_17_19_25_FullCV.RData")
inla_18_19 <- loadRData("inla_18_19_25_FullCV.RData")

# Pull out the predictions and put it in the data.frame
Index <- inla.stack.index(stk.full, tag  = "prediction")$data
lm.last$preds <- inla_18_19$summary.fitted.values[Index, "mean"]
hist(lm.last$preds)

## Load 2019 forest grid and transform pixels to ha
list.files("Shapefiles/GEE/2019_forest")
forest_2019_25 <- st_read(dsn = "Shapefiles/GEE/2019_forest", layer= "forest_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% 
  rename(grid.ID = parent_id) %>%
  mutate(forest.ha = forest*0.09)

## Transform predicted deforestation per grid to ha
## and append 2019 forest data
## pixels are 30x30m2 = 0.09 ha
lm.last.16_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09) %>%
  inner_join(forest_2019_25, by = "grid.ID")
lm.last.17_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09) %>%
  inner_join(forest_2019_25, by = "grid.ID")
lm.last.18_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09) %>%
  inner_join(forest_2019_25, by = "grid.ID")

lm.last_all_FullCV <- list(lm.last.16_19, lm.last.17_19, lm.last.18_19)
save(lm.last_all_FullCV, file="lm.last_all_FullCV.RData")

## Load final data
load("lm.last_all_FullCV.RData")
lm.last.16_19 <- lm.last_all_FullCV[[1]]
lm.last.17_19 <- lm.last_all_FullCV[[2]]
lm.last.18_19 <- lm.last_all_FullCV[[3]]

## Check if predicted deforestation is larger than remaining forest in 2019
lm.last.16_19 %>% summarise(dif = forest.ha - pred.def.ha) %>% summary()
lm.last.17_19 %>% summarise(dif = forest.ha - pred.def.ha) %>% summary()
lm.last.18_19 %>% summarise(dif = forest.ha - pred.def.ha) %>% summary()

#### Plot predicted values
lm.last.16_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

lm.last.17_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

lm.last.18_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

### Save shapefile
grd_16_19_25 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2016_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) %>%
  rename(grid.ID = parent_id) %>% mutate(grid.ID = as.factor(grid.ID)) 
  
nrow(grd_16_19_25)
names(lm.last.16_19)

lm.last.sh <- grd_16_19_25 %>% inner_join(lm.last.18_19, by = "grid.ID") %>% 
  dplyr::select(grid.ID, Long, Lat, pred.def.ha)

nrow(lm.last.sh)
st_write(lm.last.sh, "Shapefiles/Predictions/2020_Predictions.shp")

## Load shapefiles
sh_16_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2022_Predictions") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

sh_17_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2021_Predictions") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

sh_18_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2020_Predictions") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

## Sum total predicted deforestation per year
sum(sh_16_19_25$prd_df_)
sum(sh_17_19_25$prd_df_)
sum(sh_18_19_25$prd_df_)

### Rasterize values - 1
library(stars)
ra_16_19_25 <- stars::st_rasterize(sh_16_19_25)
plot(ra_16_19_25)
write_stars(ra_16_19_25, "Results/ra_16_19_25.tif")

### Rasterize values - 2
hist(lm.last.16_19$pred.def.ha)
coop.16 <- as.matrix(lm.last.16_19[, c("Long", "Lat")])
coop.17 <- as.matrix(lm.last.17_19[, c("Long", "Lat")])
coop.18 <- as.matrix(lm.last.18_19[, c("Long", "Lat")])
amazonia_2019 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2019.tif") 
ra <- raster(nrows = 100, ncols=100, ext = extent(amazonia_2019)) # higher res results in dot pattern

r_prev_16 <- rasterize(
  x = coop.16, y = ra, field = lm.last.16_19$pred.def.ha, 
  fun = median
)

r_prev_17 <- rasterize(
  x = coop.17, y = ra, field = lm.last.17_19$pred.def.ha, 
  fun = median
)

r_prev_18 <- rasterize(
  x = coop.18, y = ra, field = lm.last.18_19$pred.def.ha, 
  fun = median
)

plot(r_prev_16)
plot(r_prev_17)
plot(r_prev_18)
writeRaster(r_prev_16, file="Results/r_prev_16.tif", format= "GTiff")

## Interactive maps
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(BAMMtools)
display.brewer.all()

## Dots
## Predicted Deforestation
summary(lm.last.16_19$pred.def.ha)
pal <- colorNumeric("viridis", c(min(lm.last.16_19$pred.def.ha), max(lm.last.16_19$pred.def.ha)), na.color = "transparent")
pal <- colorNumeric("OrRd", c(min(lm.last.16_19$pred.def.ha), max(lm.last.16_19$pred.def.ha)), na.color = "transparent")

leaflet() %>% #addTiles() %>%
  #addProviderTiles(provider = "Esri") %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = coop.16[, 1], lat = coop.16[, 2],
    color = pal(lm.last.16_19$pred.def.ha),
    opacity = 0.7
  ) %>%
  addLegend("bottomright",
            pal = pal, values = values(r_prev_16),
            title = "Deforestation in 2022"
  ) %>%
  addScaleBar(position = c("bottomleft"))

#### Raster
## Predicted Deforestation static layer
summary(values(r_prev_16))
#pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")
pal <- colorNumeric(palette = "OrRd", c(min(lm.last.16_19$pred.def.ha), max(lm.last.16_19$pred.def.ha)), na.color = "transparent")
pal <- colorNumeric(palette = "Reds", c(min(lm.last.16_19$pred.def.ha), max(lm.last.16_19$pred.def.ha)), na.color = "transparent")

leaflet() %>% addTiles() %>%
  #addProviderTiles("CartoDB")  %>%
  #addProviderTiles(provider = "Esri.WorldImagery") %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_16, colors = pal, opacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = c(min(lm.last.16_19$pred.def.ha), max(lm.last.16_19$pred.def.ha)), title = "Deforestation in 2021"
  ) %>%
  addScaleBar(position = c("bottomleft"))

#### Predicted Deforestation interactive layer - rasters
min.16 <- min(values(r_prev_16), na.rm=T)
max.16 <- max(values(r_prev_16), na.rm=T)
min.17 <- min(values(r_prev_17), na.rm=T)
max.17 <- max(values(r_prev_17), na.rm=T)
min.18 <- min(values(r_prev_18), na.rm=T)
max.18 <- max(values(r_prev_18), na.rm=T)
pal.def.16 <- colorNumeric(palette = "OrRd", c(min.16, max.16), na.color = "transparent")
pal.def.17 <- colorNumeric(palette = "OrRd", c(min.17, max.17), na.color = "transparent")
pal.def.18 <- colorNumeric(palette = "OrRd", c(min.18, max.18), na.color = "transparent")

map <- leaflet() %>% addTiles() %>%
  #addProviderTiles(provider = "Esri.WorldImagery") %>%
  addRasterImage(r_prev_16, colors = pal.def.16, opacity = 0.7, group = "2022") %>%
  addRasterImage(r_prev_17, colors = pal.def.17, opacity = 0.7, group = "2021") %>%
  addRasterImage(r_prev_18, colors = pal.def.18, opacity = 0.7, group = "2020") %>%
  addLegend("bottomright",
            pal = pal.def.16,
            values = values(r_prev_16), title = "2022 Deforestation (ha)",  group = "2022") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal.def.17,
            values = values(r_prev_17), title = "2021 Deforestation (ha)",  group = "2021") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal.def.18,
            values = values(r_prev_18), title = "2020 Deforestation (ha)",  group = "2020") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLayersControl(overlayGroups = c("2022", "2021", "2020"), #baseGroups, overlayGroups
                     position = "topleft",
                     options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2022", "2021")) 

map

saveWidget(map, file="Predicted_deforestation_raster.html")

#### Predicted Deforestation interactive layer - shapefile
breaks.16 <- round(getJenksBreaks(sh_16_19_25$prd_df_, k=5, subset = NULL), 0)
breaks.17 <- round(getJenksBreaks(sh_17_19_25$prd_df_, k=5, subset = NULL), 0)
breaks.18 <- round(getJenksBreaks(sh_18_19_25$prd_df_, k=5, subset = NULL), 0)

min.16 <- round(min(sh_16_19_25$prd_df_, na.rm=T), 0)
max.16 <- round(max(sh_16_19_25$prd_df_, na.rm=T), 0)
min.17 <- round(min(sh_17_19_25$prd_df_, na.rm=T), 0)
max.17 <- round(max(sh_17_19_25$prd_df_, na.rm=T), 0)
min.18 <- round(min(sh_18_19_25$prd_df_, na.rm=T), 0)
max.18 <- round(max(sh_18_19_25$prd_df_, na.rm=T), 0)
pal.def.16 <- colorBin(palette = "OrRd", c(min.16, max.16), bins = breaks.16, na.color = "transparent")
pal.def.17 <- colorBin(palette = "OrRd", c(min.17, max.17), bins = breaks.17, na.color = "transparent")
pal.def.18 <- colorBin(palette = "OrRd", c(min.18, max.18), bins = breaks.18, na.color = "transparent")

map2 <- leaflet() %>% addTiles() %>%
  #addProviderTiles(provider = "Esri.WorldImagery") %>%
  addPolygons(data = sh_16_19_25,
              fillColor = ~pal.def.16(prd_df_), 
              fillOpacity = 0.7,
              weight = 2,
              stroke = FALSE,
              group = "2022") %>%
              #color = "red",
              #highlight = highlightOptions(weight = 10,
              #                             color = "blue",
              #                             bringToFront = TRUE))
  addPolygons(data = sh_17_19_25,
            fillColor = ~pal.def.17(prd_df_),
            fillOpacity = 0.7,
            weight = 2,
            stroke = FALSE,
            group = "2021") %>%
  addPolygons(data = sh_18_19_25,
            fillColor = ~pal.def.18(prd_df_),
            fillOpacity = 0.7,
            weight = 2,
            stroke = FALSE,
            group = "2020") %>%
  addLegend("bottomright",
            pal = pal.def.16,
            values = sh_16_19_25$prd_df_, title = "2022 Deforestation (ha)",  group = "2022") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal.def.17,
            values = sh_17_19_25$prd_df_, title = "2021 Deforestation (ha)",  group = "2021") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal.def.18,
            values = sh_18_19_25$prd_df_, title = "2020 Deforestation (ha)",  group = "2020") %>%
  addScaleBar(position = c("bottomleft")) %>%
  addLayersControl(overlayGroups = c("2022", "2021", "2020"), #baseGroups, overlayGroups
                   position = "topleft",
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2022", "2021")) 

map2

saveWidget(map2, file="Predicted_deforestation_shapefile.html")


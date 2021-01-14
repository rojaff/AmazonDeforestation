################################################################
#### Deforestation predictions for the Altamira region
## based on the selected models
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
library(viridis)
library(gridExtra)
library(scales)
source("Deforestation_modutil_v4.R")

#### Load Deforestation dataset ----
## Load data, select variables and replace pland NAs with zeros
## 2016-2019
past.def.inla <- read.csv("Subset_CV/LanMetricsLoop_Altamira_16_19_5.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_3, pland_33, pland_12, pland_41, pland_24,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit()

## 2017-2019
past.def.inla <- read.csv("Subset_CV/LanMetricsLoop_Altamira_17_19_5.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_3, pland_33, pland_12, pland_41, pland_24,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit()

## 2018-2019
past.def.inla <- read.csv("Subset_CV/LanMetricsLoop_Altamira_18_19_5.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_3, pland_33, pland_12, pland_41, pland_24,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB)) %>%
  mutate(pland_15 = replace_na(pland_15, 0)) %>%
  na.omit()

## Scale predictor variables
past.def.inla[, c(7:ncol(past.def.inla))] <- scale(past.def.inla[, c(7:ncol(past.def.inla))])

## Check data
nrow(past.def.inla) 
head(past.def.inla) 

#### Load reference 2019 data ----
lm.last <- read.csv("Subset_CV/LanMetricsLoop_Altamira_19_5.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
  mutate(grid.ID = as.factor(grid.ID)) %>%
  dplyr::select(-c(division_3, 
                   pland_3, pland_33, pland_12, pland_41, pland_24,
                   CODE, IDHM_E, IDHM_L, IDHM_R, IPS, PIB)) %>%
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
(mesh <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(0.05, 1), cutoff=0.05, offset=c(-0.2,-0.2)))$n
plot(mesh, asp=1) 
points(past.def.inla[, c("Long", "Lat")], col="red", pch=19)

## Formulas - After cross-validation
hyper.prec <- list(theta = list(prior="pc.prec", param = c(1, 0.05)))

#### Load best model formulas ----
list.files("Subset_CV/Altamira_subset_CV/Altamira_16_19_5", pattern="Excluded")
best.formula(dir="Subset_CV/Altamira_subset_CV/Altamira_16_19_5/", 
             res="INLA.RES_7", form="formulas_7")
formula <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE) + 
  len.st.roads + len.waterways + dist.rivers + 
  area.protected + area.indigenous + IDHM + POP_2018 + dem.data + 
  wi.data + dist.mining + dist.urban + dist.pa + dist.il + 
  f(field, model = spde) + f(grid.ID, model = "iid")

best.formula(dir="Subset_CV/Altamira_subset_CV/Altamira_17_19_5/", 
             res="INLA.RES_9", form="formulas_9")
formula <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE) +
  pland_15 + len.st.roads + len.rivers + 
  len.waterways + area.protected + area.indigenous + IDHM + 
  POP_2018 + PIB_PC + dem.data + wi.data + f(field, model = spde) + 
  f(grid.ID, model = "iid")

best.formula(dir="Subset_CV/Altamira_subset_CV/Altamira_18_19_5/", 
             res="INLA.RES_8", form="formulas_8")
formula <- y ~ -1 + Intercept + 
  f(inla.group(ed_3), model='rw2', hyper=hyper.prec, scale.model = TRUE) +
  len.rivers + area.indigenous + IDHM + 
  POP_2018 + PIB_PC + dem.data + slope.data + wi.data + dist.mining + 
  dist.urban + dist.pa + dist.il + f(field, model = spde) + 
  f(grid.ID, model = "iid")

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
save(m, file="Subset_CV/inla_subset_18_19_5.RData")
          
#### Model coeficients ----
## Load final (predictive) models
inla_16_19 <- loadRData("Subset_CV/inla_subset_16_19_5.RData")
inla_17_19 <- loadRData("Subset_CV/inla_subset_17_19_5.RData")
inla_18_19 <- loadRData("Subset_CV/inla_subset_18_19_5.RData")

summary(inla_16_19)
summary(inla_17_19)
summary(inla_16_19)

# Pull out the predictions and put it in the data.frame
Index <- inla.stack.index(stk.full, tag  = "prediction")$data
lm.last$preds <- inla_18_19$summary.fitted.values[Index, "mean"]
hist(lm.last$preds)

## Transform predicted deforestation per grid to ha
## pixels are 30x30m2 = 0.09 ha
lm.last.16_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09)
lm.last.17_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09) 
lm.last.18_19 <- lm.last %>% mutate(pred.def.ha = TotPix*preds*0.09) 

lm.last_all_subsets <- list(lm.last.16_19, lm.last.17_19, lm.last.18_19)
save(lm.last_all_subsets, file="Subset_CV/lm.last_all_subsets.RData")

## Load final data
load("Subset_CV/lm.last_all_subsets.RData")
lm.last.16_19 <- lm.last_all_subsets[[1]]
lm.last.17_19 <- lm.last_all_subsets[[2]]
lm.last.18_19 <- lm.last_all_subsets[[3]]

summary(lm.last.16_19$pred.def.ha)

#### Plot predicted values
lm.last.16_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

lm.last.17_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

lm.last.18_19 %>% ggplot(aes(x=Long, y=Lat, col=pred.def.ha)) + geom_point() + 
  scale_color_gradient(low="greenyellow", high="red")

### Save shapefile
grd_16_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2016_2019_5_7") %>%
  sf::st_transform(st_crs(4326)) %>% 
  rename(grid.ID = parent_id) %>% 
  mutate(grid.ID = as.factor(grid.ID)) %>%
  inner_join(lm.last.16_19, by = "grid.ID") %>% 
  dplyr::select(grid.ID, Long, Lat, pred.def.ha)

grd_17_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2017_2019_5_7") %>%
  sf::st_transform(st_crs(4326)) %>% 
  rename(grid.ID = parent_id) %>% 
  mutate(grid.ID = as.factor(grid.ID)) %>%
  inner_join(lm.last.17_19, by = "grid.ID") %>% 
  dplyr::select(grid.ID, Long, Lat, pred.def.ha)

grd_18_19_5 <- st_read(dsn = "Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_5_7") %>%
  sf::st_transform(st_crs(4326)) %>% 
  rename(grid.ID = parent_id) %>% 
  mutate(grid.ID = as.factor(grid.ID)) %>%
  inner_join(lm.last.18_19, by = "grid.ID") %>% 
  dplyr::select(grid.ID, Long, Lat, pred.def.ha)

nrow(grd_16_19_5)
names(grd_16_19_5)
st_write(grd_16_19_5, "Shapefiles/Predictions/2022_Predictions_subset.shp")
st_write(grd_17_19_5, "Shapefiles/Predictions/2021_Predictions_subset.shp")
st_write(grd_18_19_5, "Shapefiles/Predictions/2020_Predictions_subset.shp")

## Load shapefiles
sh_16_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2022_Predictions_subset") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

sh_17_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2021_Predictions_subset") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

sh_18_19_25 <- st_read(dsn = "Shapefiles/Predictions", layer= "2020_Predictions_subset") %>%
  sf::st_transform(st_crs(4326)) %>% dplyr::select(-c(grid_ID, Long, Lat))

###### Coeficient plot ----
# Pull out estimates from best models
fix.16 <- inla_16_19$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable!="Intercept") %>% mutate(year=2016)
hyp.16 <- inla_16_19$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable=='Precision for inla.group(ed_3)') %>% mutate(year=2016)

fix.17 <- inla_17_19$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable!="Intercept")  %>% mutate(year=2017)
hyp.17 <- inla_17_19$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")]  %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable=='Precision for inla.group(ed_3)') %>% mutate(year=2017)

fix.18 <- inla_18_19$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable!="Intercept")  %>% mutate(year=2018)
hyp.18 <- inla_18_19$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")]  %>%
  tibble::rownames_to_column() %>% rename(variable = rowname) %>%
  dplyr::filter(variable=='Precision for inla.group(ed_3)') %>% mutate(year=2018)

coef.table <- bind_rows(fix.16, hyp.16, fix.17, hyp.17, fix.18, hyp.18) %>% 
  mutate(variable = as.factor(variable), year = as.factor(year)) %>%
  rename(low = "0.025quant", high = "0.975quant") %>%
  mutate(variable = recode(variable, len.st.roads = "Length of state roads",
                           area.indigenous = "Indigenous lands",
                           area.protected="Protected areas", 
                           pland_15 = "Proportion of pasture",
                           dem.data="Elevation", 
                           wi.data = "Aridity (Walsh index)",
                           dist.mining = "Distance to mining areas", 
                           dist.urban = "Distance to urban areas",
                           dist.il="Distance to indigenous lands", 
                           dist.pa="Distance to protected areas", 
                           dist.rivers="Distance to rivers", dist.roads="Distance to roads", 
                           IDHM="Human development index", 
                           len.fe.roads="Length federal roads", len.rivers="Length rivers", 
                           len.waterways="Length waterways", PIB_PC="GDP per capita",
                           POP_2018="Population", 
                           'Precision for inla.group(ed_3)'="Forest edge density", 
                           slope.data="Slope"))

show_col(viridis_pal()(20))
show_col(viridis(24))
pal <- c("#21908CFF", "#3B528BFF", "#440154FF")

Coef_plot <- coef.table %>%
  ggplot(aes(y = fct_reorder(variable, mean), x = mean, 
             xmin = low, xmax = high,
             group = year, color = year)) +
  geom_vline(xintercept=0, col="black", linetype = "dashed", size=0.5) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  xlim(c(-10,20)) +
  labs(x = "Posterior Mean", y = "Variable", color = "Year") +
  scale_color_manual(values=pal) +
  #scale_color_viridis(discrete = TRUE) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_bw() + theme(
    legend.position="none",
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10, face="bold"),
    axis.text.x = element_text(size=10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

Coef_plot

#### RMSE ----
list.files("Subset_CV/Altamira_subset_CV/Altamira_16_19_5", pattern="Excluded")
INLA.RES.16_19 <- read.csv("Subset_CV/Altamira_subset_CV/Altamira_16_19_5/INLA.RES_7.csv", head=TRUE)
formulas.16_19 <- loadRData("Subset_CV/Altamira_subset_CV/Altamira_16_19_5/formulas_7.RData")
formulas.16_19[nrow(INLA.RES.16_19)]
rmse.16 <- data.frame(Formula = 1:nrow(INLA.RES.16_19), 
                      variable = c("inla.group(ed_3)", "len.st.roads", "len.waterways", "dist.rivers", 
                                     "area.protected", "area.indigenous", "IDHM", "POP_2018", "dem.data", 
                                     "wi.data", "dist.mining", "dist.urban", "dist.pa", "dist.il", "full.model")) %>%
  inner_join(INLA.RES.16_19, by="Formula") %>% mutate(year=2016)
rmse.16

list.files("Subset_CV/Altamira_subset_CV/Altamira_17_19_5", pattern="Excluded")
INLA.RES.17_19 <- read.csv("Subset_CV/Altamira_subset_CV/Altamira_17_19_5/INLA.RES_9.csv", head=TRUE)
formulas.17_19 <- loadRData("Subset_CV/Altamira_subset_CV/Altamira_17_19_5/formulas_9.RData")
formulas.17_19[nrow(INLA.RES.17_19)]
rmse.17 <- data.frame(Formula = 1:nrow(INLA.RES.17_19), 
                      variable = c("inla.group(ed_3)",  "pland_15", "len.st.roads", "len.rivers", "len.waterways",
                                   "area.protected", "area.indigenous", "IDHM", "POP_2018", "PIB_PC", "dem.data", 
                                   "wi.data", "full.model")) %>%
  inner_join(INLA.RES.17_19, by="Formula") %>% mutate(year=2017)
rmse.17

list.files("Subset_CV/Altamira_subset_CV/Altamira_18_19_5", pattern="Excluded")
INLA.RES.18_19 <- read.csv("Subset_CV/Altamira_subset_CV/Altamira_18_19_5/INLA.RES_8.csv", head=TRUE)
formulas.18_19 <- loadRData("Subset_CV/Altamira_subset_CV/Altamira_18_19_5/formulas_8.RData")
formulas.18_19[nrow(INLA.RES.18_19)]
rmse.18 <- data.frame(Formula = 1:nrow(INLA.RES.18_19), 
                      variable = c("inla.group(ed_3)", "len.rivers", "area.indigenous", 
                                   "IDHM", "POP_2018", "PIB_PC", "dem.data", 
                                   "slope.data" , "wi.data", "dist.mining", 
                                    "dist.urban", "dist.pa", "dist.il", "full.model")) %>%
  inner_join(INLA.RES.18_19, by="Formula") %>% mutate(year=2018)
rmse.18

rmse.table <- bind_rows(rmse.16, rmse.17, rmse.18) %>%
  mutate(year = as.factor(year)) %>% filter(variable!="full.model")

rmse.full <- bind_rows(rmse.16, rmse.17, rmse.18) %>%
  mutate(year = as.factor(year)) %>% filter(variable=="full.model")

fm.rmse.2016 <- rmse.full %>% filter(year==2016) %>% .$RMSE
fm.rmse.2017 <-rmse.full %>% filter(year==2017) %>% .$RMSE
fm.rmse.2018 <-rmse.full %>% filter(year==2018) %>% .$RMSE

rmse.table.dif2016 <- rmse.table %>% filter(year==2016) %>% mutate(RMSE.dif = RMSE - fm.rmse.2016)
rmse.table.dif2017 <-rmse.table %>% filter(year==2017) %>% mutate(RMSE.dif = RMSE - fm.rmse.2017)
rmse.table.dif2018 <-rmse.table %>% filter(year==2018) %>% mutate(RMSE.dif = RMSE - fm.rmse.2018)

rmse.table.dif <- bind_rows(rmse.table.dif2016, rmse.table.dif2017, rmse.table.dif2018)

pal <- c("#21908CFF", "#3B528BFF", "#440154FF")

levels(rmse.table.dif$variable)

rmse.table.dif$variable <- factor(rmse.table.dif$variable, 
                                  levels = c("inla.group(ed_3)", "dist.mining",
                                             "dist.urban", "dist.pa",
                                             "IDHM", "pland_15",
                                             "wi.data", "dem.data", "slope.data",
                                             "dist.rivers",
                                             "area.protected", 
                                             "len.rivers","len.waterways",
                                             "len.st.roads", "area.indigenous",
                                             "PIB_PC",
                                             "dist.il", "POP_2018"))

rmse.table.plot <- rmse.table.dif %>% 
  mutate(variable = recode(variable, area.indigenous = 'Indigenous lands',
                           len.st.roads = "Length of state roads",
                           area.protected="Protected areas", 
                           dem.data="Elevation", dist.il="Distance to indigenous lands", 
                           dist.pa="Distance to protected areas", 
                           dist.rivers="Distance to rivers", dist.roads="Distance to roads", 
                           IDHM="Human development index", 
                           pland_15 = "Proportion of pasture",
                           wi.data = "Aridity (Walsh index)",
                           dist.mining="Distance to mining areas",
                           dist.urban="Distance to urban areas",
                           len.fe.roads="Length federal roads", len.rivers="Length rivers", 
                           len.waterways="Length waterways", PIB_PC="GDP per capita",
                           POP_2018="Population", 
                           'inla.group(ed_3)'="Forest edge density", 
                           slope.data="Slope"))

rmse.plot <- rmse.table.plot %>% 
  ggplot(aes(x=RMSE.dif, y=fct_rev(variable), group=year, col=year)) +
  geom_point(size=3, position = position_dodge(width = 0.5)) + 
  scale_color_manual(values=pal) +  
  #scale_color_viridis(discrete = TRUE) +
  #geom_vline(xintercept=rmse.full[rmse.full$year==2016, "RMSE"], col=pal[1], linetype = "dashed", size=0.5) +
  #geom_vline(xintercept=rmse.full[rmse.full$year==2017, "RMSE"], col=pal[2], linetype = "dashed", size=0.5) +
  #geom_vline(xintercept=rmse.full[rmse.full$year==2018, "RMSE"], col=pal[3], linetype = "dashed", size=0.5) +
  labs(x = "Change in Root Mean Square Error", y = NULL, color = "Year") +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_bw() + theme(
    legend.position="right",
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=15, face="bold"))

rmse.plot
grid.arrange(Coef_plot, rmse.plot, ncol=2)
g <- grid.arrange(Coef_plot, rmse.plot, ncol=2)

ggsave(filename = "Subset_CV/Coef_plot.png", plot=g,
       device = "png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)

#### Calculate prediction error in area
RMSE16 <- INLA.RES.16_19 %>% mutate(year=2016)
RMSE17 <- INLA.RES.17_19 %>% mutate(year=2017)
RMSE18 <- INLA.RES.18_19 %>% mutate(year=2018)

RMSE.min <- bind_rows(RMSE16, RMSE17, RMSE18) %>%
  group_by(year) %>%
  summarise(min = min(RMSE)) 

past.def.2016 <- read.csv("Subset_CV/LanMetricsLoop_Altamira_16_19_5.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels)

RMSE.min %>%
  filter(year==2016) %>% .$min * median(past.def.2016$TotPix) * 0.09


##########################################################
#### Final effect plots ####
##########################################################

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
source("Utilities/Deforestation_modutil_v4.R")

#### Model coeficients ----
## Load final (predictive) models
inla_16_19 <- loadRData("inla_16_19_25_FullCV.RData")
inla_17_19 <- loadRData("inla_17_19_25_FullCV.RData")
inla_18_19 <- loadRData("inla_18_19_25_FullCV.RData")

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
  mutate(variable = recode(variable, area.indigenous = "Indigenous lands",
                           area.protected="Protected areas", 
                           dem.data="Elevation", dist.il="Distance to indigenous lands", 
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
  xlim(c(-1,1.8)) +
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
list.files("Cluster/2019_Data/Full_20var/grd_16_19_25", pattern="Excluded")
INLA.RES.16_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_16_19_25/INLA.RES_7.csv", head=TRUE)
formulas.16_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_16_19_25/formulas_7.RData")
formulas.16_19[nrow(INLA.RES.16_19)]
rmse.16 <- data.frame(Formula = 1:nrow(INLA.RES.16_19), 
                      variable = c("inla.group(ed_3)", "len.fe.roads","dist.roads", "len.rivers", 
                                   "len.waterways", "dist.rivers", "area.protected", "area.indigenous", 
                                   "POP_2018", "PIB_PC", "dem.data", "slope.data", "dist.pa", "dist.il", "full.model")) %>%
  inner_join(INLA.RES.16_19, by="Formula") %>% mutate(year=2016)
rmse.16

list.files("Cluster/2019_Data/Full_20var/grd_17_19_25", pattern="Excluded")
INLA.RES.17_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_17_19_25/INLA.RES_11.csv", head=TRUE)
formulas.17_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_17_19_25/formulas_11.RData")
formulas.17_19[nrow(INLA.RES.17_19)]
rmse.17 <- data.frame(Formula = 1:nrow(INLA.RES.17_19), 
                      variable = c("inla.group(ed_3)", "dist.roads", "len.rivers", 
                      "len.waterways", "dist.rivers", "area.protected", "area.indigenous",  
                       "PIB_PC", "slope.data", "dist.pa", "full.model")) %>%
  inner_join(INLA.RES.17_19, by="Formula") %>% mutate(year=2017)
rmse.17

list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")
INLA.RES.18_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_18_19_25/INLA.RES_9.csv", head=TRUE)
formulas.18_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_18_19_25/formulas_9.RData")
formulas.18_19[nrow(INLA.RES.18_19)]
rmse.18 <- data.frame(Formula = 1:nrow(INLA.RES.18_19), 
                      variable = c("inla.group(ed_3)", "len.fe.roads","dist.roads", "len.rivers", 
  "len.waterways", "area.protected", "area.indigenous", "IDHM", 
  "PIB_PC", "dem.data", "slope.data", "dist.il", "full.model")) %>%
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

rmse.table.dif$variable <- factor(rmse.table.dif$variable, 
                              levels = c("inla.group(ed_3)", "len.waterways", 
                                         "slope.data","dem.data", 
                                         "len.fe.roads", "len.rivers",
                                         "dist.rivers", "POP_2018", 
                                         "IDHM", "PIB_PC", "dist.pa",     
                                         "dist.il", "area.indigenous",        
                                        "area.protected","dist.roads"))

rmse.table.plot <- rmse.table.dif %>% 
  mutate(variable = recode(variable, area.indigenous = 'Indigenous lands',
                         area.protected="Protected areas", 
                         dem.data="Elevation", dist.il="Distance to indigenous lands", 
                         dist.pa="Distance to protected areas", 
                         dist.rivers="Distance to rivers", dist.roads="Distance to roads", 
                         IDHM="Human development index", 
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

ggsave(filename = "Results/Coef_plot.png", plot=g,
       device = "png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)

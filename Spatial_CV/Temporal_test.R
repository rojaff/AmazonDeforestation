
##############################################################
#### Assessment of model accuracy across temporal scales  ####
##############################################################

## Clean workspace
rm(list=ls())
gc()

library(tidyverse)
source("Utilities/Deforestation_modutil_v4.R")

#### Load best model formulas ----
list.files("Cluster/2019_Data/Full_20var/grd_15_19_25", pattern="Excluded")
INLA.RES.15_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_15_19_25/INLA.RES_7.csv", head=TRUE)
formulas.15_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_15_19_25/formulas_7.RData")
formulas.15_19[INLA.RES.15_19[1,1]] 

list.files("Cluster/2019_Data/Full_20var/grd_16_19_25", pattern="Excluded")
INLA.RES.16_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_16_19_25/INLA.RES_7.csv", head=TRUE)
formulas.16_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_16_19_25/formulas_7.RData")
formulas.16_19[INLA.RES.16_19[1,1]] 

list.files("Cluster/2019_Data/Full_20var/grd_17_19_25", pattern="Excluded")
INLA.RES.17_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_17_19_25/INLA.RES_11.csv", head=TRUE)
formulas.17_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_17_19_25/formulas_11.RData")
formulas.17_19[INLA.RES.17_19[1,1]]

list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")
INLA.RES.18_19 <- read.csv("Cluster/2019_Data/Full_20var/grd_18_19_25/INLA.RES_9.csv", head=TRUE)
formulas.18_19 <- loadRData("Cluster/2019_Data/Full_20var/grd_18_19_25/formulas_9.RData")
formulas.18_19[INLA.RES.18_19[1,1]]

RMSE15 <- INLA.RES.15_19 %>% mutate(year=2015)
RMSE16 <- INLA.RES.16_19 %>% mutate(year=2016)
RMSE17 <- INLA.RES.17_19 %>% mutate(year=2017)
RMSE18 <- INLA.RES.18_19 %>% mutate(year=2018)
RMSE <- bind_rows(RMSE15, RMSE16, RMSE17, RMSE18) %>%
  mutate(year = as.factor(year)) 

ggplot(RMSE, aes(x=year, y=RMSE)) + 
  geom_boxplot(outlier.shape = NA, fill="gray") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))

RMSE.min <- bind_rows(RMSE15, RMSE16, RMSE17, RMSE18) %>%
  group_by(year) %>%
  summarise(min = min(RMSE)) 

tp <- ggplot(RMSE.min, aes(x=year, y=min)) + 
  geom_point(size=2) + geom_line(size=2, alpha=0.75) +
  xlab("Base year") + 
  ylab("Root Mean Square Error") +
  ylim(0.010, 0.028) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))

tp

##### Fig
## create sp plot from Scale_test script
library(gridExtra)

Fig <- grid.arrange(tp, sp, ncol=2)

ggsave(filename = "Results/Fig_rmse.png", plot = Fig, 
       device = "png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 150)

#### Calculate prediction error in area
past.def.2016 <- read.csv("GridData/Temporal/LM_grd_16_19_25.csv", head=TRUE) %>%
  dplyr::rename(grid.ID = parent_id) %>% dplyr::rename(Def = loss) %>% 
  dplyr::rename(NDef = no_change) %>% dplyr::rename(TotPix = tot_pixels)

RMSE.min %>%
  filter(year==2016) %>% .$min * median(past.def.2016$TotPix) * 0.09

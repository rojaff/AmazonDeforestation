
##########################################################
### Full model containing all 20 predictor variables 
###                       &                   
###       Spatial blocks cross validation
##########################################################

## Clean workspace
rm(list=ls())
gc()

## Source utilities
library(tidyverse)
library(lares) # devtools::install_github("laresbernardo/lares")
library(lme4)
library(MuMIn)
library(sp)
library(rgdal)
library(raster)
library(gstat)
library(usdm)
library(tictoc)
library(INLA)
source("Utilities/Deforestation_modutil_v4.R")

#################### Load and prepare data ----
## Load data, select variables and replace pland NAs with zeros
## Change file name to load data for different years
past.def.inla <- read.csv("GridData/Temporal/LM_grd_16_19_25.csv", head=TRUE) %>%
  rename(grid.ID = parent_id) %>% rename(Def = loss) %>% 
  rename(NDef = no_change) %>% rename(TotPix = tot_pixels) %>%
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
names(past.def.inla)

## Check correlations
cor <- past.def.inla %>% 
  dplyr::select(-c(grid.ID, Def, NDef, TotPix, Long, Lat, pland_3))

corr_cross(cor, max_pvalue = 0.05, top = 10) 
corr_var(cor, ed_3, top = 5) 
corr_var(cor, pland_15, top = 5) 

usdm::vif(cor)

#################### GLMER Model (test) ----
names(past.def.inla)
tic()
glm.full <- glmer(cbind(Def, NDef) ~ ed_3 + pland_15 +
              len.st.roads + len.fe.roads + dist.roads +
              len.rivers + len.waterways + dist.rivers + 
              area.protected + area.indigenous + 
              IDHM + POP_2018 + PIB_PC +     
              dem.data + slope.data + wi.data + 
              dist.mining +  dist.urban + dist.pa + dist.il + 
              (1|grid.ID), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)), 
              family="binomial", data = past.def.inla)
toc() #29.205 sec
max.r(glm.full) # max correlations between predictors
max.cor(glm.full) # max correlations between predictors by predictor
overdisp_fun(glm.full)
summary(glm.full)
LRT <- drop1(glm.full, test="Chisq")
LRT

#################### INLA Model ----  
## Build mesh 
(m1 <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2)))$n
plot(m1, asp=1) 
points(past.def.inla[, c("Long", "Lat")], col="red", pch=19) # OK

## Visualize spatial blocks
set.seed(123456)
past.def.inla$fold <- kmeans(as.matrix(past.def.inla[, c("Long", "Lat")]), centers = 5)$cluster
plot(past.def.inla[, "Lat"] ~ past.def.inla[, "Long"], 
     xlab="Longitude", ylab="Latitude",
     col = past.def.inla$fold, pch=19)

tiff(filename = "Results/kmeans.tif",
     width = 20, height = 20, units = "cm",
     compression = "lzw", bg = "white", res = 300)
dev.off()

## Projector matrix (a)
A1 <- INLA::inla.spde.make.A(m1, loc = as.matrix(past.def.inla[, c("Long", "Lat")]))

## SPDE function (matern correlation)
spde1 <- INLA::inla.spde2.matern(m1, alpha=2)

## Define stack
X <- data.frame(Intercept = rep(1, nrow(past.def.inla)),
                ed_3 = past.def.inla$ed_3,
                pland_15 = past.def.inla$pland_15,
                len.st.roads = past.def.inla$len.st.roads,
                len.fe.roads = past.def.inla$len.fe.roads,
                dist.roads = past.def.inla$dist.roads,
                len.rivers = past.def.inla$len.rivers,
                len.waterways = past.def.inla$len.waterways,
                dist.rivers = past.def.inla$dist.rivers,
                area.protected = past.def.inla$area.protected,
                area.indigenous = past.def.inla$area.indigenous,
                IDHM = past.def.inla$IDHM,
                POP_2018 = past.def.inla$POP_2018, 
                PIB_PC = past.def.inla$PIB_PC,
                dem.data = past.def.inla$dem.data,
                slope.data = past.def.inla$slope.data,
                wi.data = past.def.inla$wi.data,
                dist.mining = past.def.inla$dist.mining,
                dist.urban = past.def.inla$dist.urban,
                dist.pa = past.def.inla$dist.pa,
                dist.il = past.def.inla$dist.il,
                grid.ID = past.def.inla$grid.ID) 

StackFit1 <- INLA::inla.stack(
  tag = "Fit",
  data=list(y = past.def.inla$Def, ## Number of deforested pixels
            Ntrials=past.def.inla$TotPix), ## Total number of pixels per grid
  A = list(1, A1),
  effects = list(
    X = X, 
    field = 1:spde1$n.spde)) 

## Formula
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

## Full model 
inla.doc("binomial")
inla.models()$likelihood$binomial$link

tic()
I3 <- INLA::inla(f1, # change formula here
                 family = "binomial", 
                 data = INLA::inla.stack.data(StackFit1, spde=spde1), 
                 Ntrials = INLA::inla.stack.data(StackFit1)$Ntrials,
                 # control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(A = INLA::inla.stack.A(StackFit1), link = 1, compute=TRUE))
                 #control.inla = list(int.strategy = "eb", strategy = "gaussian")) # faster but less accurate

toc() # eb & gaussian: 158.147 sec | normal: 749.589 sec (12.5min)
save(I3, file="full_model_2016.RData")

## Load models
I1 <- loadRData("full_model_2018.RData")
I2 <- loadRData("full_model_2017.RData")
I3 <- loadRData("full_model_2016.RData")

## Check results
summary(I1)
I1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] ## Fixed effects
I1$summary.fitted.values[, "mean"] ## Fitted values
I1$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] ##  Hyperparameters
I1$summary.random$field ##  Random effects
I1$marginals.fixed ## Posterior marginal distributions for predictors
I1$marginals.hyperpar ## Posterior marginal distributions for hyperparameters
plot.mar.fixed(I1) ## Custom function to plot marginal distributions of fixed effects
plot.mar.hyper(I1) ## Custom function to plot marginal distributions of hyperparameters

## Build tables with all effects
tab1 <- I1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  rename(mean_2018 = 'mean', Lci_2018 = '0.025quant', Uci_2018 = '0.975quant')
tab2 <- I2$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  rename(mean_2017 = 'mean', Lci_2017 = '0.025quant', Uci_2017 = '0.975quant')
tab3 <- I3$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  rename(mean_2016 = 'mean', Lci_2016 = '0.025quant', Uci_2016 = '0.975quant')

tabF <- bind_cols(tab1, tab2, tab3)
write.csv(tabF, file="Full_models.csv")

hyp1 <- I1$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] 
hyp2 <- I2$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] 
hyp3 <- I3$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] 

hypF <- bind_cols(hyp1, hyp2, hyp3)
write.csv(hypF, file="Full_models_hyperparameters.csv")
  
## Extract and plot residuals 
## Full residuals
RF1 <- inla.residuals.full(stack=StackFit1, model=I3, data=past.def.inla)
plot(RF1$Fitted, RF1$Residuals); abline(0,0, col="red") 
residual.plots(stack.data=X, resid.fit.df=RF1) 

## Assess OD 
N <- nrow(past.def.inla)
p <- nrow(I1$summary.fixed)
Dispersion <- sum(RF1$Residuals^2) / (N - p)
Dispersion ## No OD

## Visually assess smoother "inla.group(ed_3)"
ls(I1$summary.random)

ran.eff <- I1$summary.random$'inla.group(ed_3)'
plot(ran.eff[, 1:2], type="l",
     xlab="Effect", ylab="Smoother",
     ylim=c(-1, 1.5), main='inla.group(ed_3)')
abline(h=0, lty=3)
lines(ran.eff[, c(1,4)], lty=2)
lines(ran.eff[, c(1,6)], lty=2)

################ Step-wise model selection using spatial cross-validation----
m1 <- inla.mesh.2d(loc = past.def.inla[, c("Long", "Lat")], max.edge=c(1, 4), cutoff=1, offset=c(-0.1,-0.2))

hyper.prec <- list(theta = list(prior="pc.prec", param = c(1, 0.05)))

## Full formula 
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
toc() # LM_grd_17_19_25, 10 variables: 13.6 h 
# LM_grd_18_19_25, 20 variables: 63 h | LM_grd_17_19_25, 20 variables: 72h | LM_grd_16_19_25, 20 variables: 51h

#### Check inla.spatial.step results ----
## Excluded variables
list.files("Cluster/2019_Data/Full_20var/grd_18_19_25", pattern="Excluded")

Evars15 <- loadRData("Cluster/2019_Data/Full_20var/grd_15_19_25/Excluded_variables_7.RData")
Evars16 <- loadRData("Cluster/2019_Data/Full_20var/grd_16_19_25/Excluded_variables_7.RData")
Evars17 <- loadRData("Cluster/2019_Data/Full_20var/grd_17_19_25/Excluded_variables_11.RData")
Evars18 <- loadRData("Cluster/2019_Data/Full_20var/grd_18_19_25/Excluded_variables_9.RData")

Reduce(intersect, list(Evars16, Evars17, Evars18))
sort(table(c(Evars16, Evars17, Evars18)))
## Excluded in CV: dist.mining, dist.urban, len.st.roads, pland_15, wi.data
  

################################################################
#### Data sources ####
################################################################

rm(list=ls())
gc()

## Load libraries
library(raster)
library(sp)
library(rgdal)
library(sf)
library(pryr)
library(dplyr)
library(ggplot2)
source("LandscapeMetrics.R")

#### Deforestation ---- 
## Deforestation shapefiles created using GEE - 25km scale grids
## MapBiomas collection 5.0 : https://plataforma.mapbiomas.org/
grd_18_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2018_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
grd_17_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2017_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
grd_16_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2016_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 
grd_15_19_25 <- st_read(dsn = "/MapBiomas/Shapefiles/GEE/2019_fullgrids", layer= "changes_2015_2019_25_7") %>%
  sf::st_transform(st_crs(4326)) %>% filter(loss>0) 

## Load MapBiomas rasters
## MapBiomas collection 5.0 : https://plataforma.mapbiomas.org/
amazonia_2019 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2019.tif") 
amazonia_2018 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2018.tif") 
amazonia_2017 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2017.tif") 
amazonia_2016 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2016.tif") 
amazonia_2015 <- raster("MapBiomas/COLECAO_5_DOWNLOADS_COLECOES_ANUAL_AMAZONIA_AMAZONIA-2015.tif") 
#mem_used() # 117 MB

## Amazonia legal ----
# Terrabrasilis: http://terrabrasilis.dpi.inpe.br/download/dataset/legal-amz-aux/vector/brazilian_legal_amazon_border.zip
AL <- st_read(dsn = "Shapefiles/Terrabrasilis", layer= "brazilian_legal_amazon") %>%
  sf::st_transform(st_crs(4326))
#plot(AL, max.plot=1)

#### Roads ----
## Load, transform and crop raw shapefiles
# ImazonGeo: https://www.imazongeo.org.br/files/uploads/bases/sad/estradas_bioma_2012_geo.zip
r.imazon <- st_read(dsn = "Shapefiles/ImazonGeo", layer= "estradas_bioma_2012_geo") %>%
  sf::st_transform(st_crs(4326)) 

# MapBiomas: https://mapbiomas.org/dados-de-infraestrutura?cama_set_language=pt-BR
re.mapbiomas <- st_read(dsn = "Shapefiles/MapBiomas/rodoviario", layer= "rodovia_estadual") %>%
  sf::st_transform(st_crs(4326)) %>%
  st_intersection(AL) 

# DNIT: http://servicos.dnit.gov.br/vgeo 
rf.dnit <- st_read(dsn = "Shapefiles/DNIT", layer= "SNV_202001A") %>%
  sf::st_transform(st_crs(4326)) %>%
  st_intersection(AL)

#plot(rf.dnit, max.plot=1)

## Rbind all shapefiles
r.imazon.geom <- r.imazon %>% dplyr::select(geometry)
re.mapbiomas.geom <- re.mapbiomas %>% dplyr::select(geometry)
rf.dnit.geom <- rf.dnit %>% dplyr::select(geometry)
roads <- rbind(r.imazon.geom, re.mapbiomas.geom, rf.dnit.geom)
#plot(roads, max.plot=1)

## Save final shapefiles
st_write(re.mapbiomas.tr2, "Shapefiles/Roads/R_estaduais_mapbiomas.shp")
st_write(rf.dnit.tr2, "Shapefiles/Roads/R_federais_dnit.shp")
st_write(roads, "Shapefiles/Roads/R_all.shp")

#### Rivers ----
## Load, transform and crop raw shapefiles
# IBGE: https://www.ibge.gov.br/geociencias/cartas-e-mapas/bases-cartograficas-continuas/15759-brasil.html?=&t=downloads
riv.ibge <- st_read(dsn = "Shapefiles/IBGE/base", layer= "hid_trecho_drenagem_l") %>% ## IBGE
  sf::st_transform(st_crs(4326)) %>%
  st_intersection(AL) 

# MapBiomas: https://mapbiomas.org/dados-de-infraestrutura?cama_set_language=pt-BR 
wat.mapbiomas <- st_read(dsn = "Shapefiles/MapBiomas", layer= "Hidrovias") %>% ## MapBiomas
  sf::st_transform(st_crs(4326)) %>%
  st_intersection(AL) 

## Rbind all shapefiles
#mem_used()
riv.ibge.geom <- riv.ibge.tr %>% dplyr::select(geometry)
wat.mapbiomas.geom <- wat.mapbiomas %>% dplyr::select(geometry)
rivers <- rbind(riv.ibge.geom, wat.mapbiomas.geom)
#plot(roads, max.plot=1)

## Save final shapefile
st_write(riv.ibge.tr2, "Shapefiles/Rivers/IBGE_AL.shp")
st_write(wat.mapbiomas, "Shapefiles/Rivers/MapBiomas_AL.shp")
st_write(rivers, "Shapefiles/Rivers/Rivers_all.shp")

#### Protected areas and indigenous lands ----
## Load, transform and crop raw shapefiles
# ICBIO: http://www.icmbio.gov.br/portal/geoprocessamentos/51-menu-servicos/4004-downloads-mapa-tematico-e-dados-geoestatisticos-das-uc-s
PA <- st_read(dsn = "Shapefiles/ICMBIO", layer= "UC_fed_julho_2019") %>%
  sf::st_transform(st_crs(4326)) %>%
  sf::st_buffer(dist = 0) %>% # fix errors
  st_intersection(AL)  # dplyr::filter(anoCriacao<=2015) 

val.check(PA)
sort(unique(PA$anoCriacao))
#plot(PA, max.plot=1)

# FUNAI: http://www.funai.gov.br/index.php/shape
IL <- st_read(dsn = "Shapefiles/FUNAI", layer= "ti_sirgasPolygon") %>%
  sf::st_transform(st_crs(4326)) %>%
  st_intersection(AL)  

val.check(IL)
#plot(IL, max.plot=1)

## Save final shapefiles
st_write(PA, "Shapefiles/PA/PA_AL.shp")
st_write(IL, "Shapefiles/IL/IL_AL.shp")

#### Socio-economic variables ----
## Load IDHM data
#Atlasbrasil: http://www.atlasbrasil.org.br/ranking
IDHM <- read.csv("Pop/IDHM_2010.csv") %>%
  dplyr::select(Codmun6, Codmun7, Município, IDHM, IDHM_E, IDHM_L, IDHM_R) %>%
  rename(CODE=Codmun7) %>%
  mutate(CODE = as.factor(CODE))
head(IDHM)

## Load population data
#IBGE: https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html
Pop <- read.csv("Pop/Pop_ibge_2018.csv", dec = ".") %>%
  dplyr::select(CODE,POPULAÇÃO.ESTIMADA) %>%
  rename(POP_2018=POPULAÇÃO.ESTIMADA) %>%
  mutate(CODE = as.factor(CODE))
head(Pop)

## Load GDP data
# IBGE: https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=resultados
PIB <- read.csv("Pop/PIB_2017.csv") %>%
  dplyr::select(Código.do.Município, PIB, PIB.PC) %>%
  rename(CODE=Código.do.Município) %>%
  mutate(CODE = as.factor(CODE))
head(PIB)

## Merge datasets
Social.var <- inner_join(IDHM, Pop, by = "CODE") %>%
  inner_join(PIB, by = "CODE")

head(Social.var)
nrow(Social.var)

## Load and transform municipality shapefile
#IBGE: https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2019/Brasil/BR/br_municipios_20200807.zip
mu <- st_read(dsn = "Shapefiles/IBGE/municipios_2019", layer= "BR_Municipios_2019") %>%
  sf::st_transform(st_crs(4326)) %>%
  rename(CODE=CD_MUN) %>%
  mutate(CODE = as.factor(CODE)) %>%
  st_intersection(AL) #st_crop(st_bbox(grd_18_19_5)) 

#plot(mu, max.plot=1)

## Append municipality-level data to shapefile
nrow(mu2)
nrow(Social.var)

Soc.mu <- inner_join(mu2, Social.var, by = "CODE") %>%
  dplyr::select(c(CODE, IDHM, POP_2018, PIB.PC))

nrow(Soc.mu)
#plot(Soc.mu[, "IDHM"], max.plot = 1)

## Save final shapefile
st_write(Soc.mu, "Shapefiles/Social_var_mun/Social_var_mun.shp")

#### Abiotic variables ----
## INPE: http://www.dpi.inpe.br/amb_data/Amazonia/
## Rasters have problems and were fixed using gdalwarp: 
## https://gis.stackexchange.com/questions/269514/raster-io-error-r
## gdalwarp  IOERROR.tif IOERROR-fix.tif

## Load fixed rasters 
INPE.dem <- raster("INPE/DEM_inpe_AL-fix.tif") # http://www.dpi.inpe.br/amb_data/Amazonia/altitude_amzl.asc
INPE.slope <- raster("INPE/Slope_inpe_AL-fix.tif") # http://www.dpi.inpe.br/amb_data/Amazonia/declividade_amzl.asc
INPE.wi <- raster("INPE/WI_inpe_AL-fix.tif") # http://www.dpi.inpe.br/amb_data/Amazonia/walsh_amzl.asc

#### Mining and Urban shapefiles
## Shapefiles generated from MapBiomas collection 5.0, using GEE
## Load Mining shapefiles
mining_2019 <- st_read(dsn = "Shapefiles/Mining", layer= "mining_2019") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL)

mining_2018 <- st_read(dsn = "Shapefiles/Mining", layer= "mining_2018") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL)

mining_2017 <- st_read(dsn = "Shapefiles/Mining", layer= "mining_2017") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL) 

mining_2016 <- st_read(dsn = "Shapefiles/Mining", layer= "mining_2016") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL)

## Load Urban shapefiles
urban_2019 <- st_read(dsn = "Shapefiles/Urban", layer= "urban_2019") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL) 

urban_2018 <- st_read(dsn = "Shapefiles/Urban", layer= "urban_2018") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL) 

urban_2017 <- st_read(dsn = "Shapefiles/Urban", layer= "urban_2017") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL)

urban_2016 <- st_read(dsn = "Shapefiles/Urban", layer= "urban_2016") %>%
  sf::st_transform(st_crs(4326)) %>% #%>%  sf::st_buffer(dist = 0) 
  st_intersection(AL) 

## Save final shapefiles
st_write(mining_2019, "Shapefiles/Mining/mining_2019_AL.shp")
st_write(mining_2018, "Shapefiles/Mining/mining_2018_AL.shp")
st_write(mining_2017, "Shapefiles/Mining/mining_2017_AL.shp")
st_write(mining_2016, "Shapefiles/Mining/mining_2016_AL.shp")

st_write(urban_2019, "Shapefiles/Urban/urban_2019_AL.shp")
st_write(urban_2018, "Shapefiles/Urban/urban_2018_AL.shp")
st_write(urban_2017, "Shapefiles/Urban/urban_2017_AL.shp")
st_write(urban_2016, "Shapefiles/Urban/urban_2016_AL.shp")


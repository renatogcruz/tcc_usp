
# =====================================
# Código: preparando shapefiles
# Especialização em Estatística, 2022/2
# Essa versão: 08/11/2022      
# =====================================


# ############# #
# Preliminaries #
# ############# #

## limpando o workspace
rm(list = ls())

## carregando pacotes necessários
## Nota: aqui podemos usar tanto 'library' quanto 'require'
# carregar bases de dados de diferentes tipos
library(foreign)
# mapas e shapefiles
library(raster)
library(rgdal)
library(sp)
# manipulação de bases de dados
library(tidyverse)

## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho! [Aula 1/code?] [!]
setwd("C:/Users/uriel/Desktop/code")

## carregando DBF
dbf = read.dbf('_dta/shapefiles (original)/AREA_PONDERACAO_CENSO_2010.dbf')

## carregando indicadores do censo
dta = read.csv("_dta/GeoSES BH_IM.csv")
# gerando IDs (mesmo nome .dbf)
dta$ID_AP_SC_2 = dta$enumeration_area %% 100
# rearranjando colunas
dta = dta %>% relocate(ID_AP_SC_2, .before = UF)

## fazendo o merge do DBF com os indicadores
merge.dbf.SES = merge(dbf, dta, by = c('ID_AP_SC_2'))

## fazendo o merge com o shapefile
# carregando shapefile
shp = raster::shapefile('_dta/shapefiles (original)/AREA_PONDERACAO_CENSO_2010.shp')
# fazendo o merge
merge.shp = sp::merge(shp, merge.dbf.SES, 
                      all = FALSE, by = intersect(names(shp), names(merge.dbf.SES)))

## truncando nomes dos shapefiles para 10 caracteres
# - necessário para atender ao padrão ESRI
names(merge.shp) = abbreviate(names(merge.shp), 10, named = FALSE)

## salvando o shapefile
# função auxiliar 
# - (evita os warnings do RGDAL)
save.to.raster = function(x = NULL) {
  raster::shapefile(merge.shp, overwrite = TRUE,
                    filename = '_out/shapefiles/Belo Horizonte AP.shp')
}
# invocando função
save.to.raster()



# --
# Instalando e carregando pacotes -----------------------------------------

pacotes <- c("tidyverse",   # pacote para manipulacao de dados
             "cluster",     # algoritmo de cluster
             "dendextend",  # compara dendogramas
             "factoextra",  # algoritmo de cluster e visualizacao
             "fpc",         # algoritmo de cluster e visualizacao
             "gridExtra",   # para a funcao grid arrange
             "readxl",      # 
             "writexl")     # salvar em excel


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(corrplot)
# gráficos diversos
library(ggplot2)
# mapas e shapefiles
library(ggspatial)
library(ggsn)
library(raster)
library(rgdal)
library(sf)
library(sp)
# manipulação de bases de dados
library(tidyverse)

# --
## PLOT CLUSTER IN SHAPEFILE
## carregando shapefile de BH 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/shapefiles/belo_horizonte_AP.shp'))


# convertendo para sf
sf.obj = st_as_sf(merge.shp)

# ---------------------------
# kmeans_cluster
#Carregar base de dados - modelo kmeans_cluster: 
kmeans_fim <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/kmeans_cluster2.xlsx"))

#summary(kmeans_fim)
#glimpse(kmeans_fim)

kmeans_fim$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = kmeans_fim$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = kmeans_fim$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('kmeans')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/kmeans_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# https://spatialanalysis.github.io/workshop-notes/spatial-clustering.html
# https://stackoverflow.com/questions/62435609/plot-shapefile-with-ggplot2
# https://github.com/renatogcruz/spatial_analysis_algorithms/blob/main/SCRIPT%20-%20Modos%20de%20Visualiza%C3%A7%E2%95%9Eo.R


# ---------------------------
# average_cluster
#Carregar base de dados - modelo average_cluster: 
modelo <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/average_cluster.xlsx"))

modelo$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = modelo$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = modelo$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('average_cluster')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/average_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# ---------------------------
# complete_cluster
#Carregar base de dados - modelo complete_cluster: 
modelo <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/complete_cluster.xlsx"))

modelo$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = modelo$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = modelo$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('complete')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)


# salvando em .png
dev.print(file = '_out/figures/complete_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)




# ---------------------------
# dbscan_cluster
#Carregar base de dados - modelo dbscan_cluster: 
modelo <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/dbscan_cluster.xlsx"))

modelo$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = modelo$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = modelo$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('dbscan')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)


# salvando em .png
dev.print(file = '_out/figures/dbscan_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)




# ---------------------------
# hdbscan_cluster
#Carregar base de dados - modelo hdbscan_cluster: 
modelo <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/hdbscan_cluster2.xlsx"))

modelo$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = modelo$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = modelo$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('hdbscan')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)


# salvando em .png
dev.print(file = '_out/figures/hdbscan_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)




# ---------------------------
# single_cluster
#Carregar base de dados - modelo single_cluster: 
modelo <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/plot_shapefile/single_cluster.xlsx"))

modelo$cluster
## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`cluster` = modelo$cluster  # muda-se aqui

# classificando em quintis
sf.obj$`cluster_cat` = modelo$`cluster_cat` #quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `cluster_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('single')) +
  # legenda
  guides(fill = guide_legend('cluster')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/single_shapefile.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


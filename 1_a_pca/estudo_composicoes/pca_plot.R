## limpando o workspace
rm(list = ls())

#--
## carregando pacotes necessários (install.packages())
## Nota: aqui podemos usar tanto 'library' quanto 'require'
# matrizes de correlação
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
# Carregando a base de dados

## diretório de trabalho
setwd("C:/Users/Renato/OneDrive/github/_tcc/1_a_pca/estudo_composicoes")

#--
## carregando funções auxiliares
source("_src/src.R")

#--
## carregando dados: GeoSES Belo Horizonte (intra-municipal)
## - indicadores do censo por ÁREA DE PONDERAÇÃO
## - os dados são carregados no objeto "dta"
dta = read.csv("_dta/geoSES_BH_IM.csv")

#--
## visualizando os dados
## (e checando para ver se está tudo OK)
#View(dta)

#--
# checando IDs/unidades de observação (áreas de ponderação/APs)
dta$enumeration_area
class(dta$enumeration_area)

# se as APs parecem ser as mesmas: 
# - aumentar a precisão do display
# - o padrão/default é options(digits = 7)
options(digits = 16)
dta$enumeration_area

# retornando ao padrão
options(digits = 7)

# gerando IDs
# - os IDs são os 2 últimos dígitos das APs
dta$ID = dta$enumeration_area %% 100
dta$ID

# rearranjando colunas
# - usando o 'pipe operator'
dta = dta %>% relocate(ID, .before = UF)
# - código equivalente (s/ o operador)
dta = relocate(ID, .data = dta, .before = UF)

#--
## população de BH por AP
dta[, c('ID', 'N')]
# população total de BH
sum(dta$N)

# --
## carregando shapefile de BH 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('_out/shapefiles/belo_horizonte_AP.shp'))

# convertendo para sf
sf.obj = st_as_sf(merge.shp)

# classificando N em quintis
sf.obj$N.cat = quant.class(sf.obj$N, c = 5)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = N.cat)) +
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
  ggtitle(paste('População por AP em Belo Horizonte')) +
  # legenda
  guides(fill = guide_legend('Número de habitantes')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_N.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# ################################## #
# Médias, correlações e covariâncias #
# ################################## #

#  -- 
# variáveis finais 
lab = c('P_SEM_INST',  # Education 
        'P_ENSSUP',    # Education
        'M_DENSMORA',  # Poverty
        'P_POBREZA',   # Poverty
        'M_RENDDOM',   # Income
        'P_IDOSO10SM', # Wealth
        'P_ALVSREV',   # Material deprivation
        'P_TUDOADEQ')  # Material deprivation

## matriz de observações
X = dta[, lab]
# matriz de observações padronizadas
Z = scale(X)

## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_EDUCATION.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = -PCA$scores[, 1] # muda-se aqui

#summary(PCA$scores[, 1])
#glimpse(PCA$scores[, 1])

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 3)

# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
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
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'PuBu') + #'RdYlBu'
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)


# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)
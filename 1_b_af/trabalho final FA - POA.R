# ############ #
# Preliminares #
# ############ #

## limpando o workspace
rm(list = ls())

## carregando pacotes necessários
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
# distribuição Normal multivariada
library(MASS)
library(mclust)
# kde multivariada
library(ks)
# classificação e regressão
library(caret)

## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho!
setwd("C:/Users/Gabriel Barros/Documents/Curso de Especializa��o em Estat�stica/An�lise Multivariada/Trabalho Final/code")

## carregando funções auxiliares
source("_src/src.R")

## carregando dados: GeoSES Belo Horizonte (intra-municipal)
## - indicadores do censo por *áreas de ponderação*
## - os dados são carregados no objeto "dta"
dta = read.csv("_dta/GeoSES PA_IM.csv")

## visualizando os dados
# gerando IDs
# - os IDs são os 3 últimos dígitos das APs
dta$ID = dta$enumeration_area %% 1000
dta$ID
# ordenando por ID
dta = dta[order(dta$ID), ]
# rearranjando colunas
dta = dta %>% relocate(ID, .before = UF)

## carregando shapefile de POA 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('_out/shapefiles/Porto Alegre AP.shp'))
# convertendo para sf
sf.obj = st_as_sf(merge.shp)
# ordenando por AP
sf.obj = sf.obj[order(sf.obj$COD_AREA_P), ]

# ######################################## #
# Exemplo Prático. Indicadores de Educação #
# ######################################## #

## labels
lab = c('P_GRAD', 'P_MEST', 'P_DOUTOR', 
        'P_SEM_INST', 'P_FUND', 'P_ENSMED', 
        'P_ENSSUP')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# m = 1
FA1 = factanal(X, factors = 1, scores = 'regression')
FA1
# m = 2
FA2 = factanal(X, factors = 2, scores = 'regression')
FA2
# m = 3
FA3 = factanal(X, factors = 3, scores = 'regression')
FA3
# Nota: o modelo não é bem definindo para m >= 4
# -- NOT RUN: factanal(X, factors = 4, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_EDUCATION.txt')
print(FA1)
cat('\n')
print(FA2)
cat('\n')
print(FA3)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Educa��o')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
# Exemplo Prático. Indicadores de Mobilidade #
# ########################################## #

## labels
lab = c('P_OUTROMUNC', 'P_CASADIA', 'P_ATE5',
        'P_6A30', 'P_1A2', 'P_MAISDE2')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_MOBILITY.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# m = 1
FA1 = factanal(X, factors = 1, scores = 'regression')
FA1
# m = 2
FA2 = factanal(X, factors = 2, scores = 'regression')
FA2
# m = 3
FA3 = factanal(X, factors = 3, scores = 'regression')
FA3
# Nota: o modelo não é bem definindo para m >= 4
# -- NOT RUN: factanal(X, factors = 4, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_MOBILITY.txt')
print(FA1)
cat('\n')
print(FA2)
cat('\n')
print(FA3)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Mobilidade')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_MOBILITY.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ####################################### #
# Exemplo Prático. Indicadores de Pobreza #
# ####################################### #

## labels
lab = c('M_DENSMORA', 'P_POBREZA', 'P_PPI_POBREZA', 
        'P_BOLSA_FAM', 'P_OUTROSPROG')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_POVERTY.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# m = 1
FA1 = factanal(X, factors = 1, scores = 'regression')
FA1
# m = 2
FA2 = factanal(X, factors = 2, scores = 'regression')
FA2
# Nota: o modelo não é bem definindo para m >= 3
# -- NOT RUN: factanal(X, factors = 3, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_POVERTY.txt')
print(FA1)
cat('\n')
print(FA2)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Pobreza')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_POVERTY.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
# Exemplo Prático. Indicadores de Deprivação #
# ########################################## #

## labels
lab = c('P_ALVSREV', 'P_REDE_ESG', 'P_REDE_AGUA', 
        'P_LIXO', 'P_ENERGIA', 'P_TV',
        'P_MAQLAV', 'P_GELADEIRA', 'P_MAQTVGEL',
        'P_CELULAR', 'P_COMP_INT', 'P_CELCOMPINT',
        'P_MOTO', 'P_CARRO', 'P_ADEQ',
        'P_TUDOADEQ', 'P_NEM_MOTO_CARRO', 'P_SO_MOTO',
        'P_SO_CARRO')

## matriz de observações
X = dta[, lab]

## Nota: os EMVs do modelo FA não são definidos
# -- para X, pois X é quase singular
# -- NOT RUN: factanal(X, factors = 1, scores = 'regression')
det(cor(X))
# solução: eliminar as colunas linearmente independentes de X
# -- aqui, removemos variáveis com |cor| >= .99
index.collinear = findCorrelation(cor(X), 
                        names = FALSE, cutoff = 0.99, verbose = TRUE)
index.collinear
X.new = subset(X, select = -index.collinear)

## scree plot
screeplot(princomp(scale(X.new)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_DEPRIVATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA 
# m = 1
FA1 = factanal(X.new, factors = 1, scores = 'regression')
FA1
# m = 2
FA2 = factanal(X.new, factors = 2, scores = 'regression')
FA2
# m = 3
FA3 = factanal(X.new, factors = 3, scores = 'regression')
FA3
# m = 4
FA4 = factanal(X.new, factors = 4, scores = 'regression')
FA4
# Nota: o modelo não convergiu para m = 5
# -- possível problema com os valores iniciais
# -- NOT RUN: factanal(X.new, factors = 5, scores = 'regression')
# m = 6
FA6 = factanal(X.new, factors = 6, scores = 'regression')
FA6
# Nota: o modelo não convergiu para m = 7
# -- possível problema com os valores iniciais
# -- NOT RUN: factanal(X.new, factors = 7, scores = 'regression')
# Nota: o modelo não convergiu para m = 8
# -- possível problema com os valores iniciais
# -- NOT RUN: factanal(X.new, factors = 8, scores = 'regression')
# Nota: o modelo não convergiu para m = 9
# -- possível problema com os valores iniciais
# -- NOT RUN: factanal(X.new, factors = 9, scores = 'regression')
# Nota: o modelo não é bem definindo para m/ >= 10
# -- NOT RUN: factanal(X, factors = 4, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_DEPRIVATION.txt')
print(FA1)
cat('\n')
print(FA2)
cat('\n')
print(FA3)
cat('\n')
print(FA4)
cat('\n')
print(FA6)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Depriva��o')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_DEPRIVATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ##################################### #
# Exemplo Prático. Indicadores de Renda #
# ##################################### #

## labels
lab = c('M_RENDDOM')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_INCOME.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# Nota: o modelo FA precisa de no mínimo 3 variáveis
# -- NOT RUN: factanal(X, factors = 1, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_INCOME.txt')
cat('')
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = X
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Renda')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_INCOME.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ####################################### #
# Exemplo Prático. Indicadores de Riqueza #
# ####################################### #

## labels
lab = c('P_ALUG100', 'P_BANH4OUMAIS', 'P_IDOSO10SM')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_WEALTH.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# m = 1
FA1 = factanal(X, factors = 1, scores = 'regression')
FA1
# Nota: o modelo não é bem definindo para m >= 2
# -- NOT RUN: factanal(X, factors = 2, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_WEALTH.txt')
print(FA1)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Riqueza')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_WEALTH.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
# Exemplo Prático. Indicadores de Segregação #
# ########################################## #

## labels
lab = c('ICE_renda', 'ICE_edu', 'ICE_renda_preto',
        'ICE_renda_ppi', 'ICE_branco_renda')

## matriz de observações
X = dta[, lab]

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_SEGREGATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

## FA
# m = 1
FA1 = factanal(X, factors = 1, scores = 'regression')
FA1
# m = 2
FA2 = factanal(X, factors = 2, scores = 'regression')
FA2
# Nota: o modelo não é bem definindo para m >= 3
# -- NOT RUN: factanal(X, factors = 3, scores = 'regression')
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/FA_SEGREGATION.txt')
print(FA1)
cat('\n')
print(FA2)
sink()

## mapa: scores da FA (1 fator apenas)
# salvando os scores no objeto sf
sf.obj$Factor1 = FA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
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
  ggtitle(paste('Scores do 1� fator: Segrega��o')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)
print(p)
# salvando em .png
dev.print(file = '_out/figures/figMap_FA_SEGREGATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


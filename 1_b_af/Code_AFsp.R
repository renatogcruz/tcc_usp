
# =====================================
# Seminários Estatística Multivariada
# Especialização em Estatística, 2022/2
# Essa versão: 12/12/2022      
# =====================================


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
setwd("~/Documents/Especialização Estatística/7. Multivariada - 20221112-20221217/Trabalho/Bases de dados/AF/code")

## carregando funções auxiliares
source("_src/src.R")

## carregando dados: GeoSES São Paulo (intra-municipal)
## - indicadores do censo por *áreas de ponderação*
## - os dados são carregados no objeto "dta"
dta = read.csv("_dta/GeoSES SP_IM.csv")

## visualizando os dados
# gerando IDs
# - os IDs são os 3 últimos dígitos das APs
dta$ID = dta$enumeration_area %% 1000
dta$ID
# ordenando por ID
dta = dta[order(dta$ID), ]
# rearranjando colunas
dta = dta %>% relocate(ID, .before = UF)

## carregando shapefile de SP 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('_out/shapefiles/Sao Paulo AP.shp'))
# convertendo para sf
sf.obj = st_as_sf(merge.shp)
# ordenando por AP
sf.obj = sf.obj[order(sf.obj$CD_APONDE), ]


# ############################################ #
# Exemplo #1. Distribuição Normal Multivariada #
# ############################################ #

## simulando os dados
n = 5000
set.seed(42)
mu = matrix(c(0, 0, 0), 
            nrow = 3, ncol = 1, byrow = TRUE
)
Sigma = matrix(c(1, 0.9, -0.2,
                 0.9, 1, 0,
                 -0.2, 0, 1),
               nrow = 3, ncol = 3, byrow = TRUE
)
X = mvrnorm(n, mu, Sigma)
mu
Sigma
X

## densidades univariadas
# x1
plot(density(X[, 1]), 
     main = expression(paste('Densidade de ', X[1])))
rug(X[, 1])
dev.print(file = '_out/figures/figExample1_X1dens.png',
          device = png, width = 1024, height = 768, res = 2*72)
# x2
plot(density(X[, 2]), 
     main = expression(paste('Densidade de ', X[2])))
rug(X[, 2])
dev.print(file = '_out/figures/figExample1_X2dens.png',
          device = png, width = 1024, height = 768, res = 2*72)
# x3
plot(density(X[, 3]), 
     main = expression(paste('Densidade de ', X[3])))
rug(X[, 3])
dev.print(file = '_out/figures/figExample1_X3dens.png',
          device = png, width = 1024, height = 768, res = 2*72)

## densidades multivariadas
# X1 vs. X2
plot(X[, c(1, 2)],
     main = expression(paste('Densidade de ', X[1], ' vs. ', X[2])),
     xlab = expression(X[1]),
     ylab = expression(X[2]))
par(new = TRUE)
plot(kde(X[, c(1, 2)]), 
     xlab = '', xaxt = 'n',
     ylab = '', yaxt = 'n')
dev.print(file = '_out/figures/figExample1_X1vsX2dens.png',
          device = png, width = 1024, height = 768, res = 2*72)
# X1 vs. X3
plot(X[, c(1, 3)],
     main = expression(paste('Densidade de ', X[1], ' vs. ', X[3])),
     xlab = expression(X[1]),
     ylab = expression(X[3]))
par(new = TRUE)
plot(kde(X[, c(1, 3)]), 
     xlab = '', xaxt = 'n',
     ylab = '', yaxt = 'n')
dev.print(file = '_out/figures/figExample1_X1vsX3dens.png',
          device = png, width = 1024, height = 768, res = 2*72)
# X2 vs. X3
plot(X[, c(2, 3)],
     main = expression(paste('Densidade de ', X[2], ' vs. ', X[3])),
     xlab = expression(X[2]),
     ylab = expression(X[3]))
par(new = TRUE)
plot(kde(X[, c(2, 3)]), 
     xlab = '', xaxt = 'n',
     ylab = '', yaxt = 'n')
dev.print(file = '_out/figures/figExample1_X2vsX3dens.png',
          device = png, width = 1024, height = 768, res = 2*72)

## EMVs
mu = as.matrix(colMeans(X))
round(mu, 4)
Sigma = cov(X)
round(Sigma, 4)


# ################################## #
# Exemplo #2. Estimação no modelo FA #
# ################################## #

## simulando os dados
n = 1000
set.seed(42 + 6)
mu = matrix(c(0, 0, 0, 0, 0, 0), 
            nrow = 6, ncol = 1, byrow = TRUE
)
Sigma = matrix(c(1, 0.9, 0.9, 0, 0, 0,
                 0.9, 1, 0.9, 0, 0, 0,
                 0.9, 0.9, 1, 0, 0, 0,
                 0, 0, 0, 1, 0.9, 0.9,
                 0, 0, 0, 0.9, 1, 0.9,
                 0, 0, 0, 0.9, 0.9, 1),
               nrow = 6, ncol = 6, byrow = TRUE
)
X = mvrnorm(n, mu, Sigma)

mu
Sigma
X
## estimando as cargas fatoriais
FA = factanal(X, factors = 2)
loadings(FA, cutoff = 0.05)

## Nota: os resultados são equivalentes
## -- para X e para Z
Z = scale(X)
FA.Z = factanal(Z, factors = 2)
loadings(FA.Z, cutoff = 0.05)

## resultado completo da FA
FA

## decomposição da variância total
Vf = colSums(FA$loadings[]^2)
round(Vf, 3)
VR = sum(FA$uniquenesses)
round(VR, 3)
VT = sum(Vf) + VR
round(VT, 3)
VP = 1e2*Vf/VT
round(VP, 3)
round(sum(VP), 3)
round(1e2*VR/VT, 3)

## teste GOF
n = 1000
p = 6
m = 2
Lambda = FA$loadings[]
Psi = diag(FA$uniquenesses)
# estatística de teste
M = n + 1 - (1/6)*(2*p + 5) - (2/3)*m
f.obj = as.numeric(FA$criteria[1])
T.obs = M*f.obj
round(T.obs, 3)
# graus de liberdade
nu = (1/2)*((p - m)^2) - (1/2)*(p + m)
nu
# nível crítico
round(qchisq(0.05, df = nu), 3)
# p-valor
round(1 - pchisq(1.81, df = nu), 3)

## scree plot
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figExample1_ScreePlot.png',
          device = png, width = 1024, height = 768, res = 2*72)



# ######################################## #
# Exemplo Prático. Indicadores de Educação #
# ######################################## #

## labels
lab = c('P_GRAD', 'P_MEST', 'P_DOUTOR', 
        'P_SEM_INST', 'P_FUND', 'P_ENSMED', 
        'P_ENSSUP')

## matriz de observações
X = dta[, lab]
X

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
sink(file = '_out/tables/FA_EDUCATION.txt')
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
  ggtitle(paste('Scores do 1º fator: Educação')) +
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
dev.print(file = '_out/figures/figMap_FA_EDUCATION1.png',
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
sink(file = '_out/tables/FA_MOBILITY.txt')
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
  ggtitle(paste('Scores do 1º fator: Mobilidade')) +
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
sink(file = '_out/tables/FA_POVERTY.txt')
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
  ggtitle(paste('Scores do 1º fator: Pobreza')) +
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
sink(file = '_out/tables/FA_DEPRIVATION.txt')
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
  ggtitle(paste('Scores do 1º fator: Deprivação')) +
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
sink(file = '_out/tables/FA_INCOME.txt')
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
  ggtitle(paste('Scores do 1º fator: Renda')) +
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
sink(file = '_out/tables/FA_WEALTH.txt')
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
  ggtitle(paste('Scores do 1º fator: Riqueza')) +
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
sink(file = '_out/tables/FA_SEGREGATION.txt')
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
  ggtitle(paste('Scores do 1º fator: Segregação')) +
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


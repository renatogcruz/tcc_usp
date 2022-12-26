# =============================================
# ANÁLISE PRELIMINAR - Cidade de Belo Horizonte
# 
# Versão: 2022/12/26     
# =============================================

# --
## limpando o workspace
rm(list = ls())


# --
##  instalando e carregando pacotes

pacotes <- c("corrplot",   # matrizes de correlação
             "ggplot2",    # gráficos diversos
             "ggspatial",  # mapas e shapefiles
             "ggsn",       # mapas e shapefiles
             "raster",     # mapas e shapefiles
             "rgdal",      # mapas e shapefiles
             "sf",         # mapas e shapefiles
             "sp",         # mapas e shapefiles
             "tidyverse",  # manipulação de bases de dados
             "kableExtra")  


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# --
## diretório de trabalho
setwd("C:/Users/Renato/OneDrive/github/_tcc/pca/analise_preliminar")

# --
## carregando funções auxiliares
source("_src/src.R")



# --
## carregando dados: GeoSES Belo Horizonte (intra-municipal)

## - indicadores do censo por *ÁREA DE PONDERAÇÃO*
dta = read.csv("_dta/geoSES_BH_IM.csv")

# --
## visualizando os dados
dta %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)


# --
# gerando IDs
# - os IDs são os 2 últimos dígitos das APs
dta$ID = dta$enumeration_area %% 100
dta$ID


# rearranjando colunas
dta = dta %>% relocate(ID, .before = UF)

## visualizando os dados com IDs
dta %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)


# --
## carregando shapefile de BH 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('_out/shapefiles/belo_horizonte_AP.shp'))

# convertendo para sf
sf.obj = st_as_sf(merge.shp)


# ######################################## #
#          Indicadores de Educação         #
# ######################################## #

# --
## labels
# Appendix 1 - Input variables for creating GeoSES - (aux/GeoSES variables.docx)

lab = c('P_GRAD',     
        'P_MEST', 
        'P_DOUTOR', 
        'P_SEM_INST', 
        'P_FUND', 
        'P_ENSMED', 
        'P_ENSSUP')


# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# --
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_EDUCATION.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")

# salvando em .png
dev.print(file = '_out/figures/figScreePlot_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

# --
# mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = - PCA$scores[, 1]   # invertendo o sinal

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Educação')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)


# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_EDUCATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
#          Indicadores de Mobilidade         #
# ########################################## #

## labels
lab = c('P_OUTROMUNC', 
        'P_CASADIA', 
        'P_ATE5',
        'P_6A30', 
        'P_1A2', 
        'P_MAISDE2')


# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_MOBILITY.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_MOBILITY.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")

# salvando em .png
dev.print(file = '_out/figures/figScreePlot_MOBILITY.png',
          device = png, width = 1024, height = 768, res = 2*72)

# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = - PCA$scores[, 1] # invertendo o sinal

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Mobilidade')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_MOBILITY.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ####################################### #
#          Indicadores de Pobreza         #
# ####################################### #

## labels
lab = c('M_DENSMORA', 
        'P_POBREZA', 
        'P_PPI_POBREZA', 
        'P_BOLSA_FAM', 
        'P_OUTROSPROG')

# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_POVERTY.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_POVERTY.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")


# salvando em .png
dev.print(file = '_out/figures/figScreePlot_POVERTY.png',
          device = png, width = 1024, height = 768, res = 2*72)


# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = - PCA$scores[, 1] # invertendo os sinais

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Pobreza')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_POVERTY.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
#           Indicadores de Privação          #
# ########################################## #

## labels
lab = c('P_ALVSREV', 'P_REDE_ESG', 'P_REDE_AGUA', 
        'P_LIXO', 'P_ENERGIA', 'P_TV',
        'P_MAQLAV', 'P_GELADEIRA', 'P_MAQTVGEL',
        'P_CELULAR', 'P_COMP_INT', 'P_CELCOMPINT',
        'P_MOTO', 'P_CARRO', 'P_ADEQ',
        'P_TUDOADEQ', 'P_NEM_MOTO_CARRO', 'P_SO_MOTO',
        'P_SO_CARRO')


# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_DEPRIV.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# --
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_PRIVATION.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")

# salvando em .png
dev.print(file = '_out/figures/figScreePlot_PRIVATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = - PCA$scores[, 1] # invertendo sinal

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Deprivação')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_PRIVATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ##################################### #
# Exemplo Prático. Indicadores de Renda #
# ##################################### #

## labels
lab = c('M_RENDDOM')

# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = mean(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
as.matrix(var(dta[, lab]))

# matriz de correlações
corr = as.matrix(1)
rownames(corr) = 'M_RENDDOM'
colnames(corr) = 'M_RENDDOM'

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_INCOME.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_INCOME.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em .png
dev.print(file = '_out/figures/figScreePlot_INCOME.png',
          device = png, width = 1024, height = 768, res = 2*72)

# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Renda')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_INCOME.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ####################################### #
# Exemplo Prático. Indicadores de Riqueza #
# ####################################### #

## labels
lab = c('P_ALUG100', 
        'P_BANH4OUMAIS', 
        'P_IDOSO10SM')


# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_WEALTH.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# --
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_WEALTH.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")

# salvando em .png
dev.print(file = '_out/figures/figScreePlot_WEALTH.png',
          device = png, width = 1024, height = 768, res = 2*72)


# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Riqueza')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_WEALTH.png',
          device = png, width = 1024, height = 768, res = 2*72)


# ########################################## #
# Exemplo Prático. Indicadores de Segregação #
# ########################################## #

## labels
lab = c('ICE_renda', 
        'ICE_edu', 
        'ICE_renda_preto',
        'ICE_renda_ppi', 
        'ICE_branco_renda')


# -------------------------------------------
# Médias, correlações e covariâncias
# -------------------------------------------


# --
# vetor de médias
# Nota: invocar apenas 'mu' printa o vetor como data frame
# - as.matrix(mu) printa o vetor como um vetor coluna
mu = colMeans(dta[, lab])
mu
as.matrix(mu)

# matriz de covariâncias
cov = cov(dta[, lab])
cov

# matriz de correlações
corr = cor(dta[, lab])
corr

# corrplot
corrplot(corr,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = T,
         # número de dígitos
         number.digits = 2
)

# salvando em .png
dev.print(file = '_out/figures/figCorrPlot_SEGREGATION.png',
          device = png, width = 1024, height = 768, res = 1.2*72)


# -------------------------------------------
# PCA 
# -------------------------------------------

# --
## matriz de observações
X = dta[, lab]

# --
# matriz de observações padronizadas
Z = scale(X)

# --
## PCA
PCA = princomp(Z)
summary(PCA)

# --
# matriz de pesos
PCA$loadings[]

# salvando os resultados em um arquivo de texto
sink(file = '_out/output/PCA_SEGREGATION.txt')
print(summary(PCA))
cat('\n')
print(PCA$loadings[])
sink()

# --
## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
k <- sum((PCA$sdev ^ 2) > 1)
abline(h = 1, lty = 2, col="red")
abline(v = k, lty = 2, col="red")

# salvando em .png
dev.print(file = '_out/figures/figScreePlot_SEGREGATION.png',
          device = png, width = 1024, height = 768, res = 2*72)

# --
## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)

# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]

# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)

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
  ggtitle(paste('Scores da 1ª Componente da PCA: Segregação')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() +
  # rosa dos ventos (ggsn)
  ggsn::north(sf.obj)

print(p)

# salvando em .png
dev.print(file = '_out/figures/figMap_PCA_SEGREGATION.png',
          device = png, width = 1024, height = 768, res = 2*72)


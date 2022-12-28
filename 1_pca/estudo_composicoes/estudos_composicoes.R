
# =============================================
# ESTUDO DE COMPOSIÇÃO - Belo Horizonte
# 
# criação: 2022/12/28
# Versão: 2022/12/28     
# =============================================

# --
## limpando o workspace
rm(list = ls())

# --
# Instalando e carregando pacotes -----------------------------------------

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
# Aplicação do algoritmo --------------------------------------------------

# --
# Carregando a base de dados

## diretório de trabalho
setwd("C:/Users/Renato/OneDrive/github/_tcc/1_pca/estudo_composicoes")

#--
## carregando funções auxiliares
source("_src/src.R")

#--
## carregando dados: GeoSES Belo Horizonte (intra-municipal)
## - indicadores do censo por ÁREA DE PONDERAÇÃO
## - os dados são carregados no objeto "dta"
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
#  Estudo de composição de banco de dados  #
# ######################################## #

# REGRA: manter apenas 10 variáveis

# sem variavel de 'segregation'

lab = c('P_SEM_INST',  # Education 
        'P_ENSSUP',    # Education
        'P_ATE5',      # Mobility
        'P_MAISDE2',   # Mobility
        'M_DENSMORA',  # Poverty
        'P_POBREZA',   # Poverty
        'M_RENDDOM',   # Income
        'P_IDOSO10SM', # Wealth
        'P_ALVSREV',   # Material deprivation
        'P_TUDOADEQ')  # Material deprivation


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

# --
# matriz de covariâncias
cov = cov(dta[, lab])
cov

# --
# matriz de correlações
corr = cor(dta[, lab])
corr

# --
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
dev.print(file = '_out/figures/1_figCorrPlot.png',
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
sink(file = '_out/output/1_PCA.txt')
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
dev.print(file = '_out/figures/1_figScreePlot.png',
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
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
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
dev.print(file = '_out/figures/1_figMap_PCA.png',
          device = png, width = 1024, height = 768, res = 2*72)



# Teste de Bartlett de Esfericidade
Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

# Resultado do Teste de Esfericidade:
Bartlett.sphericity.test(X)

# --
# salvando os resultados em um arquivo de texto
sink(file = '_out/output/Bartletts_test.txt')
print(Bartlett.sphericity.test(X))
sink()











# -------------------------------------------
# PCA 2.0
# -------------------------------------------


# --
# rearranjando colunas
dta = dta %>% relocate(ID, .before = UF)
banco_original1 = dta[,lab]

# --
# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
nf_std <- nf[, lab] %>% 
  scale() %>%                         # faz a padronização
  data.frame()                        # converte o banco em um dataframe novamente



# --
# Rodando a PCA:
afpc <- prcomp(nf_std)        # aula teórica se resume nesta única linha
summary(afpc)                 # similar ao .describe() do Pandas


# --
# O objeto afpc possui os seguintes componentes:
afpc$sdev
afpc$rotation
afpc$center

# sdev: corresponde à raiz quadrada dos eigenvalues, ou seja, os desvios-padrão dos
# componentes principais.

# rotation: corresponde à matriz de tamanho jxj de eigenvectors, em que j 
# representa a quantidade de variáveis da base de dados.

# center: médias de cada variável utilizadas para após a padronização.

# scale: desvios-padrão de cada variável utilizadas para a padronização.

# --
# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
data.frame(afpc$rotation) %>%
  mutate(var = names(nf[, lab])) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Scree Plot - apenas ignorar os warnings
# para escolher quantos fatores serão retidos. Mas vamos utilizar critério de kaiser
ggplotly(
  fviz_eig(X = afpc,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3")
)

#--
# non-conformable arguments
# Extraindo as Cargas Fatoriais
k <- sum((afpc$sdev ^ 2) > 1) #número de variáveis presentes na base de dados
k <- 2 ## ATENÇÃO - valor mínimo é 2

cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])


# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)
#--

# Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)


# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()


# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev 
colnames(scores_fatoriais) <- colnames(nf_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)

# --
# outra viagem
# Proposta da construção de um ranking ------------------------------------

# Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
# fatorias
score_D1 <- scores_fatoriais[1,] # selecionando a primeira linha
score_D1

score_D2 <- scores_fatoriais[2,] # selecionando a segunda linha
score_D2


# Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(nf_std, 1, function(x) x * score_D1))
F2 <- t(apply(nf_std, 1, function(x) x * score_D2))

F1
F2

# Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
# visto que os scores fatoriais das observações mais fortes são, por padrão, 
# presentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * 1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

F2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)

# Importando as colunas de fatores F1 e F2
nf_std["Fator1"] <- F1$fator1
nf_std["Fator2"] <- F2$fator2
res_ACP = nf_std[,c('Fator1', 'Fator2')]

# Criando um ranking pela soma ponderada dos fatores por sua vari?ncia
# compartilhada:

# Calculando a vari?ncia compartilhada
var_compartilhada <- (afpc$sdev ^ 2/sum(afpc$sdev ^ 2))
var_compartilhada

nf_std %>%
  mutate(pontuacao = Fator1 * var_compartilhada[1] +
           Fator2 * var_compartilhada[2]) -> nf

# Visualizando o ranking final
nf_std %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 10)


# Teste de Bartlett de Esfericidade
Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

# Resultado do Teste de Esfericidade:
Bartlett.sphericity.test(banco_original1)


# Gráfico Colorido com as Contribuições
fviz_pca_var(res_ACP, col.var="contrib", gradient.cols = c("red","yellow","green"), repel = TRUE # Avoid text overlapping) 
)

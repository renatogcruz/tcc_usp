########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

library(tidyverse)   #pacote para manipulacao de dados
library(cluster)     #algoritmo de cluster
library(dendextend)  #compara dendogramas
library(factoextra)  #algoritmo de cluster e visualizacao
library(fpc)         #algoritmo de cluster e visualizacao
library(gridExtra)   #para a funcao grid arrange
library(readxl)


########################################
#
#   Brincando e comparando todos os métodos com dbscan
#
########################################

#   Guias e tutoriais
########################################
#
# https://www.r-bloggers.com/2021/04/cluster-analysis-in-r/
#
########################################

#Carregar base de dados: 
censo <- as.data.frame(read_excel("fatores_e_ranking_final_2.xlsx"))
scores <- as.data.frame(read_excel("scores_fatoriais_2.xlsx"))

#pegando os dados que usaremos
fatores <- censo %>% 
  select(ID, Fator1, Fator2)

#para visualizar no plano
fatores %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2),
             size = 3)

#Transformar o nome 
rownames(fatores) <- fatores[,1]
fatores <- fatores[,-1]

#Padronizar variaveis
fatores_pad <- scale(fatores) # aqui não precisava fazer isso

#para visualizar no plano
fatores_pad %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2),
             size = 3)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(fatores_pad, method = "euclidean")


# --
### método hiearquico

#Calcular o Cluster
# "single linkage"
# "complete linkage"
# "average linkage"
# https://github.com/renatogcruz/clustering_algorithms/blob/main/cluster_2022/An%C3%A1lise%20de%20Cluster/SCRIPT%20-%20Pa%C3%ADses.R
cluster.hierarquico <- hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#criando grupos
grupo_hierarquico <- cutree(cluster.hierarquico, k = 3)
table(grupo_hierarquico)

#transformando em data frame a saida do cluster
grupo_hierarquico <- data.frame(grupo_hierarquico)

#juntando com a base original
fatores_fim <- cbind(fatores, grupo_hierarquico)

#visualizando em cores os clusters
fatores_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(grupo_hierarquico)),
             size = 3) + 
  ggtitle("Método hiearquico")+
  theme(plot.title = element_text(hjust = 0.5))


# --
### método k-means

# Método de Elbow para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
fviz_nbclust(fatores_pad, kmeans, method = "wss", k.max = 5)

#Calcular o Cluster
cluster.k3 <- kmeans(fatores_pad, centers = 3)

#criando grupos
grupo_kmeans3 <- data.frame(cluster.k3$cluster)

#juntando com a base original
fatores_fim <- cbind(fatores_fim, grupo_kmeans3)

#visualizando em cores os clusters
fatores_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(cluster.k3.cluster)),
             size = 3) + 
  ggtitle("Método k-means")+
  theme(plot.title = element_text(hjust = 0.5))


### método dbscan

#Calcular o Cluster
dbscan <- fpc::dbscan(fatores_pad,eps = 0.56, MinPts = 3)

fatores_fim$dbscan <- dbscan$cluster

#visualizando em cores os clusters
fatores_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(dbscan)),
             size = 3) + 
  ggtitle("Método dbscan")+
  theme(plot.title = element_text(hjust = 0.5))

# --
# Análise de variância de um fator (ANOVA)
# ANOVA da variável 'fator1'
summary(anova_fator1 <- aov(formula = Fator1 ~ dbscan,
                                data = fatores_fim))
# ANOVA da variável 'fator2'
summary(anova_fator2 <- aov(formula = Fator2 ~ dbscan,
                            data = fatores_fim))

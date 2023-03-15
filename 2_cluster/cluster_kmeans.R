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
# https://www.r-bloggers.com/2021/04/cluster-analysis-in-r/
#
########################################

#Carregar base de dados: 
censo <- as.data.frame(read_excel("fatores_e_ranking_final_2.xlsx"))


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
kmeans_fim <- cbind(fatores_fim, grupo_kmeans3)

#visualizando em cores os clusters
kmeans_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(cluster.k3.cluster)),
             size = 3) + 
  ggtitle("Método k-means")+
  theme(plot.title = element_text(hjust = 0.5))

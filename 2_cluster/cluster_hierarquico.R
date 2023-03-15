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

########################################
#
#   CARREGANDO E TRABALHANDO OS DADOS
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
#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(fatores_pad, method = "euclidean")

########################################
#
#   método hiearquico 
#
########################################

# --
### distancia = single linkage
# Single Linkage - Vizinho mais próximo - Estruturas geométricas diferentes, 
# mas é incapaz de delinear grupos pouco separados

cluster_single.hierarquico <- hclust(distancia, method = "single")

# Dendrograma
plot(cluster_single.hierarquico, cex = 0.6, hang = -1)

#criando grupos
single_hierarquico <- cutree(cluster_single.hierarquico, k = 3)
table(single_hierarquico)

#transformando em data frame a saida do cluster
single_hierarquico <- data.frame(single_hierarquico)

#juntando com a base original
single_fim <- cbind(fatores, single_hierarquico)

#visualizando em cores os clusters
single_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(single_hierarquico)),
             size = 3) + 
  ggtitle("Método hiearquico - single")+
  theme(plot.title = element_text(hjust = 0.5))


# --
### distancia = complete linkage
# Complete Linkage - Vizinho mais longe - Clusters de mesmo diâmetro e isolam 
# os outliers nos primeiros passos

cluster_complete.hierarquico <- hclust(distancia, method = "complete")

# Dendrograma
plot(cluster_complete.hierarquico, cex = 0.6, hang = -1)

#criando grupos
complete_hierarquico <- cutree(cluster_complete.hierarquico, k = 3)
table(complete_hierarquico)

#transformando em data frame a saida do cluster
complete_hierarquico <- data.frame(complete_hierarquico)

#juntando com a base original
complete_fim <- cbind(fatores, complete_hierarquico)

#visualizando em cores os clusters
complete_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(complete_hierarquico)),
             size = 3) + 
  ggtitle("Método hiearquico - complete linkage")+
  theme(plot.title = element_text(hjust = 0.5))


# --
### distancia = Average Linkage
# Average Linkage - Média - Clusters de mesma variância interna, produzindo 
# melhores partições

cluster_average.hierarquico <- hclust(distancia, method = "average")

# Dendrograma
plot(cluster_average.hierarquico, cex = 0.6, hang = -1)

#criando grupos
average_hierarquico <- cutree(cluster_average.hierarquico, k = 3)
table(average_hierarquico)

#transformando em data frame a saida do cluster
average_hierarquico <- data.frame(average_hierarquico)

#juntando com a base original
average_fim <- cbind(fatores, average_hierarquico)

#visualizando em cores os clusters
average_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(average_hierarquico)),
             size = 3) + 
  ggtitle("Método hiearquico - verage Linkage")+
  theme(plot.title = element_text(hjust = 0.5))

# --
### distancia = Ward´s Method
# Ward´s Method - Cluster com o mesmo número de itens, baseado nos princípios de análises de variâncias

cluster_ward.hierarquico <- hclust(distancia, method = "ward.D")

# Dendrograma
plot(cluster_ward.hierarquico, cex = 0.6, hang = -1)
# salvando em .png
dev.print(file = '_out/figures/dendrograma_ward.png',
          device = png, width = 1024, height = 768, res = 2*72)

#criando grupos
ward_hierarquico <- cutree(cluster_ward.hierarquico, k = 3)
table(ward_hierarquico)

#transformando em data frame a saida do cluster
ward_hierarquico <- data.frame(ward_hierarquico)

#juntando com a base original
ward_fim <- cbind(fatores, ward_hierarquico)

#salvando xlsx modelo final
write_xlsx(df,"_out/output/ward_cluster.xlsx")

#visualizando em cores os clusters
ward_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(ward_hierarquico)),
             size = 3) + 
  ggtitle("Método hiearquico - Ward´s Method")+
  theme(plot.title = element_text(hjust = 0.5))

# salvando em .png
dev.print(file = '_out/figures/ward.png',
          device = png, width = 1024, height = 768, res = 2*72)

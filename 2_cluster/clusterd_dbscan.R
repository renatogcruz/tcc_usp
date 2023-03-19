########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

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
# https://www.r-bloggers.com/2021/04/cluster-analysis-in-r/
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7940#:~:text=dbscan()%20shows%20a%20statistic,%3D%200.15%20and%20MinPts%20%3D%205.
# https://medium.com/towards-data-science/density-based-clustering-dbscan-vs-hdbscan-39e02af990c7
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

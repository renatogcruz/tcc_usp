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
#
########################################

# --
# Carregando a base de dados


## diretório de trabalho
setwd("C:/Users/Renato/OneDrive/github/_tcc/2_cluster")


# --
#Carregar base de dados - modelo PCA final: 
censo <- as.data.frame(read_excel("_dta/fatores_e_ranking_final_2.xlsx"))


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
fviz_nbclust(fatores_pad, kmeans, method = "wss", k.max = 5) + 
  theme(axis.text.x = element_text(size = 12))  +
  geom_point(group = 1, size = 3, color = "blue")
  ggtitle("Método de Elbow - k-means")+
    theme(plot.title = element_text(hjust = 0.5))


# salvando em .png
dev.print(file = '_out/figures/elbow_kmeans.png',
          device = png, width = 1024, height = 768, res = 2*72)

#Calcular o Cluster
cluster.k3 <- kmeans(fatores_pad, centers = 3)

#criando grupos
grupo_kmeans3 <- data.frame(cluster.k3$cluster)

#juntando com a base original
kmeans_fim <- cbind(fatores, grupo_kmeans3)

# --
#salvando xlsx modelo final
write_xlsx(kmeans_fim,"_out/output/kmeans_cluster.xlsx")


#visualizando em cores os clusters
kmeans_fim %>% ggplot() +
  geom_point(aes(x = Fator1,
                 y = Fator2,
                 color = as.factor(cluster.k3.cluster)),
             size = 3) + 
  ggtitle("Método k-means")+
  theme(plot.title = element_text(hjust = 0.5))

# salvando em .png
dev.print(file = '_out/figures/kmeans_plot.png',
          device = png, width = 1024, height = 768, res = 2*72)


# --
# Análise de variância de um fator (ANOVA)
# ANOVA da variável 'fator1'
summary(anova_fator1 <- aov(formula = Fator1 ~ cluster.k3$cluster,
                            data = kmeans_fim))

sink(file = '_out/output/anova_fator1_kmeans.txt')
print(summary(anova_fator1 <- aov(formula = Fator1 ~ cluster.k3$cluster,data = kmeans_fim)))
sink()

# ANOVA da variável 'fator2'
summary(anova_fator2 <- aov(formula = Fator2 ~ cluster.k3$cluster,
                            data = kmeans_fim))

sink(file = '_out/output/anova_fator2_kmeans.txt')
print(summary(anova_fator2 <- aov(formula = Fator2 ~ cluster.k3$cluster,data = kmeans_fim)))
sink()


# --
## PLOT CLUSTER IN SHAPEFILE
## carregando shapefile de BH 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/shapefiles/belo_horizonte_AP.shp'))


# convertendo para sf
sf.obj = st_as_sf(merge.shp)

#Carregar base de dados - modelo PCA final: 
kmeans_fim <- as.data.frame(read_excel("C:/Users/Renato/OneDrive/github/_tcc/2_cluster/_out/output/kmeans_cluster.xlsx"))

#summary(kmeans_fim)
#glimpse(kmeans_fim)

## salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = kmeans_fim$`Comp. 1`  # muda-se aqui

# classificando em quintis
sf.obj$`Comp. 1_cat` = kmeans_fim$`Comp. 1_cat`#quant.class(sf.obj$`Comp. 1`, c = 3)

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

# https://spatialanalysis.github.io/workshop-notes/spatial-clustering.html
# https://stackoverflow.com/questions/62435609/plot-shapefile-with-ggplot2
# https://github.com/renatogcruz/spatial_analysis_algorithms/blob/main/SCRIPT%20-%20Modos%20de%20Visualiza%C3%A7%E2%95%9Eo.R






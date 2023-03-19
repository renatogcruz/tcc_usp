########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

# --
# Instalando e carregando pacotes -----------------------------------------

pacotes <- c("tidyverse",   # pacote para manipulacao de dados
             "cluster",     # algoritmo de cluster
             "dbscan",      # algoritmo de cluster dbscan
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
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
# https://medium.com/towards-data-science/density-based-clustering-dbscan-vs-hdbscan-39e02af990c7
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
### método hdbscan

# Para executar o algoritmo HDBSCAN, simplesmente passe o conjunto de dados e o 
# valor do parâmetro (único) 'minPts' para a função hdbscan
cl <- hdbscan(fatores_pad, minPts = 5)
cl

# Os resultados 'flat' são armazenados no membro 'cluster'. Os pontos de ruído 
# recebem um valor de 0, portanto, incremente em 1.
plot(fatores_pad, col=cl$cluster+1, pch=20)

# Os resultados correspondem a noções intuitivas de como os aglomerados 
# 'semelhantes' podem parecer quando se manifestam em formas arbitrárias

# --
# DBSCAN Hierárquico - HDBSCAN
# O objeto HDBSCAN resultante contém uma representação hierárquica de todos os 
# agrupamentos DBSCAN* possíveis. Esta representação hierárquica é compactamente 
# armazenada no familiar membro 'hc' do objeto HDBSCAN resultante, no mesmo 
# formato de objetos de agrupamento hierárquicos tradicionais formados usando o 
# método 'hclust' do pacote stats.

cl$hc

# Observe que, embora este objeto esteja disponível para uso com qualquer um dos 
# métodos que funcionam com objetos 'hclust', o método de distância que o HDBSCAN 
# usa (distância de acessibilidade mútua, consulte [2]) não é um método disponível 
# da função hclust. Essa hierarquia, denominada “hierarquia HDBSCAN*” em [3], 
# pode ser visualizada usando o método de plotagem integrado do pacote stats

plot(cl$hc, main="Dendrograma - HDBSCAN Hierarchy") + 
  theme(plot.title = element_text(hjust = 0.5))

# --
# DBSCAN* vs cortar a árvore HDBSCAN*
# Como o nome indica, o fascinante sobre a hierarquia HDBSCAN* é que qualquer 
# 'corte' global é equivalente a executar DBSCAN* (DBSCAN sem pontos de borda) 
# no limite de corte da árvoree p s (assumindo o mesmom i n Pts _  configuração 
# de parâmetro foi usada). Mas isso pode ser verificado manualmente? Usando uma 
# função modificada para distinguir o ruído usando a distância do núcleo como 0 
# (uma vez que o método cutree estatístico não atribui singletons com 0), os 
# resultados podem ser mostrados como idênticos.


cl <- hdbscan(fatores_pad, minPts = 5)
check <- rep(F, nrow(fatores_pad)-1)
core_dist <- kNNdist(fatores_pad, k=5-1)

## cutree doesn't distinguish noise as 0, so we make a new method to do it manually 
cut_tree <- function(hcl, eps, core_dist){
  cuts <- unname(cutree(hcl, h=eps))
  cuts[which(core_dist > eps)] <- 0 # Use core distance to distinguish noise
  cuts
}

eps_values <- sort(cl$hc$height, decreasing = T)+.Machine$double.eps ## Machine eps for consistency between cuts 
for (i in 1:length(eps_values)) { 
  cut_cl <- cut_tree(cl$hc, eps_values[i], core_dist)
  dbscan_cl <- dbscan(fatores_pad, eps = eps_values[i], minPts = 5, borderPoints = F) # DBSCAN* doesn't include border points
  
  ## Use run length encoding as an ID-independent way to check ordering
  check[i] <- (all.equal(rle(cut_cl)$lengths, rle(dbscan_cl$cluster)$lengths) == "TRUE")
}
print(all(check == T))

# --
# Árvore Simplificada
# A hierarquia HDBSCAN* é útil, mas para conjuntos de dados maiores ela pode se 
# tornar excessivamente incômoda, pois cada ponto de dados é representado como 
# uma folha em algum lugar da hierarquia. O objeto hdbscan vem com uma poderosa 
# ferramenta de visualização que traça a hierarquia 'simplificada' (consulte [2] 
# para obter mais detalhes), que mostra as alterações em todo o cluster em um 
#número infinito dee p s limiares. É a visualização padrão enviada pelo método 
#'plot'
#plot(cl)
#plot(cl, gradient = c("yellow", "orange", "red", "blue"))
plot(cl, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
plot(cl, gradient = as.factor(cl$cluster+1), show_flat = T)


# -- 
# Pontuações de estabilidade de cluster
# Observe que as pontuações de estabilidade correspondem aos rótulos na árvore 
# condensada, mas as atribuições de cluster no elemento membro do cluster não 
# correspondem aos rótulos na árvore condensada. Além disso, observe que essas 
# pontuações representam as pontuações de estabilidade antes da passagem para 
# cima na árvore que atualiza as pontuações com base nos filhos.

print(cl$cluster_scores)

# As 'probabilidades' de membros de pontos individuais estão no elemento de 
# membro de probabilidades
head(cl$membership_prob)

# Eles podem ser usados para mostrar o 'grau de associação ao cluster', por 
# exemplo, plotando pontos com transparências que correspondem aos seus graus de associação.
plot(fatores_pad, col=cl$cluster+1, pch=21)
colors <- mapply(function(col, i) adjustcolor(col, alpha.f = cl$membership_prob[i]), 
                 palette()[cl$cluster+1], seq_along(cl$cluster))
points(fatores_pad, col=colors, pch=20)


# Pontuação atípica global-local de hierarquias
# Uma publicação recente em um periódico sobre HDBSCAN vem com uma nova medida de 
# outlier que calcula uma pontuação de outlier de cada ponto nos dados com base 
# em propriedades locais e globais da hierarquia, definida como Global-Local 
# Outlier Score from Hierarchies (GLOSH) [4]. Um exemplo disso é mostrado abaixo, 
# onde, ao contrário das probabilidades de pertença, a opacidade do ponto 
# representa a quantidade de “outlier” que o ponto representa. Tradicionalmente, 
# outliers são geralmente considerados como observações que se desviam do valor 
# esperado de sua distribuição subjacente presumida, onde a medida de desvio que 
# é considerada significativa é determinada por algum valor limite estatístico.

# Nota: Devido à distinção feita de que os pontos de ruído, pontos que não são 
# atribuídos a nenhum cluster, devem ser considerados na definição de um outlier, 
# as pontuações de outlier calculadas não são apenas as pontuações inversamente 
# proporcionais às probabilidades de associação.
top_outliers <- order(cl$outlier_scores, decreasing = T)[1:10]
colors <- mapply(function(col, i) adjustcolor(col, alpha.f = cl$outlier_scores[i]), 
                 palette()[cl$cluster+1], seq_along(cl$cluster))
plot(fatores_pad, col=colors, pch=20)
text(fatores_pad[top_outliers, ], labels = top_outliers, pos=3)



cl$cluster
#juntando com a base original
hdbscan_fim <- cbind(fatores, cl$cluster)

# --
#salvando xlsx modelo final
write_xlsx(complete_fim,"_out/output/hdbscan_cluster.xlsx")



# -- 
# Referências
# Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). Um algoritmo 
# baseado em densidade para descobrir clusters em grandes bancos de dados 
# espaciais com ruído. Instituto de Ciência da Computação, Universidade de Munique. 
# Anais da 2ª Conferência Internacional sobre Descoberta de Conhecimento e 
# Mineração de Dados (KDD-96).
# Campello, Ricardo JGB, Davoud Moulavi, Arthur Zimek e Jörg Sander. “Uma estrutura 
# para extração ideal semi-supervisionada e não supervisionada de clusters de 
# hierarquias.” Mineração de dados e descoberta de conhecimento 27, no. 3 (2013): 344-371.
# Campello, Ricardo JGB, Davoud Moulavi e Joerg Sander. “Clustering baseado em 
# densidade com base em estimativas de densidade hierárquica.” Na Conferência 
# Pacífico-Ásia sobre Descoberta de Conhecimento e Mineração de Dados, pp. 160-172. 
# Springer Berlim Heidelberg, 2013.
# Campello, Ricardo JGB, Davoud Moulavi, Arthur Zimek e Jörg Sander. “Estimativas 
# de densidade hierárquica para agrupamento de dados, visualização e detecção de 
# outliers.” Transações ACM sobre Descoberta de Conhecimento a partir de Dados 
# (TKDD) 10, no. 1 (2015): 5.
# Karypis, George, Eui-Hong Han e Vipin Kumar. “Chameleon: agrupamento hierárquico 
# usando modelagem dinâmica.” Computador 32, não. 8 (1999): 68-75.
# Hahsler M, Piekenbrock M, Doran D (2019). “dbscan: Agrupamento rápido baseado em 
# densidade com R.” Journal of Statistical Software, 91(1), 1-30. doi: 10.18637/jss.v091.i01
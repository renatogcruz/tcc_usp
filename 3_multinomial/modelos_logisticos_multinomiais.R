# --
# Pacotes utilizados
pacotes <- c("plotly",
             "tidyverse",
             "knitr",
             "kableExtra",
             "fastDummies",
             "rgl",
             "car",
             "reshape2",
             "jtools",
             "lmtest",
             "caret",
             "pROC",
             "ROCR",
             "nnet",
             "magick",
             "cowplot",
             "readxl")

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
# REGRESSÃO LOGÍSTICA MULTINOMIAL 

# --
## diretório de trabalho
setwd("C:/Users/Renato/OneDrive/github/_tcc/3_multinomial")

#--
## carregando dados
kmeans_cluster <- as.data.frame(read_excel("_dta/kmeans_cluster.xlsx"))

#  -- 
# Visualizando a base de dados kmeans_cluster
kmeans_cluster %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#  -- 
# transformando y em quali
kmeans_cluster$complete_hierarquico <- factor(kmeans_cluster$complete_hierarquico)

# --
# Estatísticas descritivas univariadas da base de dados
summary(kmeans_cluster)
glimpse(kmeans_cluster)

# --
# Apontando a categoria de referência
kmeans_cluster$complete_hierarquico <- relevel(kmeans_cluster$complete_hierarquico,
                                               ref = 1)

# --
# Estimação do modelo - função multinom do pacote nnet
modelo_kmeans <- multinom(formula = complete_hierarquico ~ Fator1 + Fator2, 
                            data = kmeans_cluster)


# Parâmetros do modelo_kmeans
summary(modelo_kmeans)

# --# LL do modelo_kmeans
logLik(modelo_kmeans)

# --
# A função summ do pacote jtools não funciona para objetos de classe 'multinom'. 
# Logo, vamos definir uma função Qui2 para se extrair a estatística geral do 
# modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

# --
# Estatística geral do modelo_kmeans
Qui2(modelo_kmeans) 

# estat. diferente de zero, pelo menos um beta é estatist. diferente de zero 
# (há modelo)

# --
# Na Regressão Logística Multinomial, o R quebra a lógica de relatórios que, 
# normalmente, oferece para os GLM. Também é preciso notar que a linguagem 
# básica  não consegue rodar esse tipo de regressão, sendo necessário o pacote
# nnet. Além do mais, não são fornecidas as estatísticas z de Wald, nem os
# p-values das variáveis da modelagem.

# Explicando a lógica do R para a Logística Multinomial:

# 1 - Foram estabelecidas *labels* para as categorias da variável dependente: 
# 'não chegou atrasado', 'chegou atrasado à primeira aula' e 'chegou atrasado à
# segunda aula';

# 2 - Foi comandado que a categoria de referência seria a categoria 'não chegou
# atrasado', e isso explica o porquê dela não aparecer no relatório gerado;

# 3 - O relatório é dividido em duas partes: 'Coefficients' e 'Std. Errors'. 
# Cada linha da seção 'Coefficients' informa um logito para cada categoria da
# variável dependente, com exceção da categoria de referência. Já a seção 
# 'Std. Errors' informa o erro-padrão de cada parâmetro em cada logito.

# Para calcular as estatísticas z de Wald, há que se dividir os valores da 
# seção 'Coefficients' pelos valores da seção 'Std. Errors.' Assim, temos que:  

zWald_modelo_kmeans <- (summary(modelo_kmeans)$coefficients / 
                            summary(modelo_kmeans)$standard.errors)

zWald_modelo_kmeans

qnorm(0.025, lower.tail = F) 
# ou os valores são maior que 1.96 ou maiores 
# (ninguem fica dentro da região que contem o zero)


# --
# Porém, ainda faltam os respectivos p-values. Assim, os valores das probabilidades 
# associadas às abscissas de uma distribuição normal-padrão é dada pela função
# pnorm(), considerando os valores em módulo - abs(). Após isso, multiplicamos 
# por dois os valores obtidos para considerar os dois lados da distribuição
# normal padronizada (distribuição bicaudal). Desta forma, temos que:
round((pnorm(abs(zWald_modelo_kmeans), lower.tail = F) * 2), 4)

# --
# ATENÇÃO **********************************************************************
# verificar se são estatisticamente significantes


# --
# A EFETIVIDADE GERAL DO MODELO
# Adicionando as prováveis ocorrências de evento apontadas pela modelagem à 
# base de dados
AtrasadoMultinomial$predicao <- predict(modelo_kmeans, 
                                     newdata = AtrasadoMultinomial, 
                                     type = "class")

# --
# Visualizando a nova base de dados AtrasadoMultinomial com a variável 'predicao'
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

attach(AtrasadoMultinomial)

# --
# Criando uma tabela para comparar as ocorrências reais com as predições
EGM <- as.data.frame.matrix(table(atrasado, predicao))

# --
#Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# --
#Eficiência global do modelo
acuracia <- (round((sum(diag(table(complete_hierarquico, predicao))) / 
                      sum(table(complete_hierarquico, predicao))), 2))

acuracia

# --
# PLOTAGENS DAS PROBABILIDADES

#Adicionando à base de dados as probabilidades em razão de cada categoria:
levels(kmeans_cluster$complete_hierarquico)

kmeans_cluster[c(1,2,3)] <- modelo_kmeans$fitted.values

#Plotagem das smooth probability lines para a variável 'dist'
ggplotly(
  kmeans_cluster %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c(1,
                                          2,
                                          3))) %>% 
    ggplot() +
    geom_smooth(aes(x = dist, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Distância Percorrida",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)


# --
# CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO 

# Adicionando os valores previstos de probabilidade da base de dados
kmeans_cluster$phat <- modelo_kmeans$fitted.values

# Visualizando a base de dados com a variável 'phat'
kmeans_cluster %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
confusionMatrix(table(predict(modelo_kmeans, type = "response") >= 0.5,
                      kmeans_cluster$complete_hierarquico == 1)[2:1, 2:1])


# Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_atrasos,
                                                         type = "response") >= 0.5,
                                                 Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_atrasos,
                                                          type = "response") >= 0.5,
                                                  Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo_atrasos,
                                                    type = "response") >= 0.5,
                                            Atrasado$atrasado == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 12)

# Matriz de confusão para cutoff = 0.3
confusionMatrix(table(predict(modelo_kmeans, type = "response") >= 0.3,
                      kmeans_cluster$complete_hierarquico == 1)[2:1, 2:1])

# Matriz de confusão para cutoff = 0.7
confusionMatrix(table(predict(modelo_kmeans, type = "response") >= 0.7,
                      kmeans_cluster$complete_hierarquico == 1)[2:1, 2:1])

# --
# IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE

# Tentaremos estabelecer um critério que iguale a probabilidade de acerto
# daqueles que chegarão atrasados (sensitividade) e a probabilidade de acerto
# daqueles que não chegarão atrasados (especificidade).

# ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
# FORMA o procedimento garante a maximização da acurácia do modelo!

# função prediction do pacote ROCR
predicoes <- prediction(predictions = modelo_kmeans$fitted.values, 
                        labels = kmeans_cluster$complete_hierarquico) 
# a função prediction, do pacote ROCR, cria um objeto com os dados necessários
# para a futura plotagem da curva ROC.

# função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
# A função peformance(), do pacote ROCR, extrai do objeto 'predicoes' os 
# dados de sensitividade e de especificidade para a plotagem.

# Desejamos os dados da sensitividade e de especificidade. Então, devemos
# digitar os seguintes códigos::

sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

# Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

# Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
# e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
# base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
# frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

# Visualizando o novo data frame dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

# Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

# --
# CONSTRUÇÃO DA CURVA ROC

# função roc do pacote pROC
ROC <- roc(response = Atrasado$atrasado, 
           predictor = modelo_atrasos$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)



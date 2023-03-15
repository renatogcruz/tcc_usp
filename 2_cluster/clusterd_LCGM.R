# set working directory (location of data)
#set.wd(…)

# open package
install.packages('lcmm')
library(lcmm)

# definir a semente para o gerador de números aleatórios, para que os 
# resultados possam ser reproduzido:
set.seed(2002)

# execute modelos com 1-4 classes, cada uma com 100 partidas aleatórias,
# usando o modelo de 1 classe para definir valores iniciais iniciais:
lcga1 <- hlme(y ~ time, subject = "ID", ng = 1, data = mydata)
lcga2 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m=hlme(y ~ time, subject = "ID",
                           ng = 2, data = mydata, mixture = ~ time))
lcga3 <- gridsearch(rep = 100, maxiter = 10, minit = lcga1,
                    m=hlme(y ~ time, subject = "ID",
                           ng = 3, data = mydata, mixture = ~ time))

# faça tabela com resultados para os 3 modelos:
summarytable(lcga1, lcga2, lcga3)
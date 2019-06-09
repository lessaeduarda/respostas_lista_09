# Lista 09


  # Questão 1:

# Xi e Yi - Correlação positiva
# Zi e Yi - Correlação negativa

## letra a)

#Se a correlação entre Xi e Zi é zero, esta última não terá efeito sobre a
# análise da relação entre Xi e Yi e, portanto, é possível omiti-la. Esta 
# situação, no entanto, é improvável de ocorrer. 

## letra b)

# Se a correlação entre Xi e Zi for positiva, é necessário analisar a 
# relação entre Xi e Yi controlando para Zi, visto que a correlação positiva
# entre estas duas VI's pode superestimar o verdadeiro efeito de Xi em Yi. 

## letra c)

# Se a correlação entre Xi e Zi for negativa, o efeito de Xi sobre Yi pode
# ter sido subestimado; assim como no item anterior, é necessário controlar para
# Zi para ter uma melhor aproximação do real efeito de Xi sobre Yi.


---
  
  
  # Questão 2:
  
# No primeiro modelo (A), o salário médio dos professores de escolas públicas 
# nos EUA (nos níveis de elementary e secondary school) é a variável dependente.
# A porcentagem de residentes dos estados americanos com diploma universitário 
# é a variável independente. O efeito encontrado é significativo, com o 
# ^alfa = 28768.01; ^beta = 704.02; erro padrão =  140.22 e p-valor < 0.05. 
# Estes resultados apontam que quando a VI = 0, o valor da VD é representado por
# ^alfa e que o aumento de 1 unidade na VI é responsável por um aumento de 704.2 
# unidades da VD (neste caso, provavelmente, a unidade da VD é dólar americano).
# É possível concluir que o aumento de 1% no total(%) de residentes com diploma
# universitário em um estado é responsável por um aumento de cerca de 704.02
# dólares na média salarial dos professores deste mesmo estado. O R^2 aponta que
# 0.34 da variação na VD (salário médio) deve-se ao efeito da VI 
# (% de residentes com diploma).

# No segundo modelo (B), a VD é a mesma (o salário médio dos professores) e a 
# VI é a renda per capita. O efeito encontrado é significativo, com o ^alfa = 
# 21168.11; ^beta = 0.68; erro padrão = 0.11 e p-valor < 0.05. Estes resultados
# apontam que quando a VI = 0, o valor da VD é representado por ^alfa e que o 
# aumento de uma unidade na VI é responsável por um aumento de 0.68 na VD. O R^2
# aponta que 0.47 da variação na VD (salário médio) deve-se ao efeito da VI 
# (renda per capita).


---
  
  
  # Questão 3:

# No terceiro modelo (C) são analisados os efeitos de cada uma das VIs sobre a 
# VD, ou seja, o efeito do percentual de habitantes com diploma universitário
# sobre o salário médio dos professores, controlado pela renda per capita; assim
# como o efeito da renda per capita sobre o salário médio dos professores,
# controlado pelo percentual de habitantes com diploma. Para a primeira VI, o 
# efeito encontrado não foi significativo, já para a segunda (renda), o ^beta
# de 0.66 apresentou p-valor < 0.05. O R^2 apresentou o mesmo valor encontrado
# no segundo modelo, de 0.47. 

# É possível notar, ao comparar os modelos, que ao controlar pela renda, o 
# efeito da primeira VI (%diploma) deixa de ser significativo e também que a sua
# inclusão, neste terceiro modelo (C), não alterou a capacidade explicativa 
# obtida no segundo (B). Para a VI renda, com o controle para a VI % diploma, o 
# valor do ^beta diminuiu um pouco, o que é um indício de que o valor do ^beta 
# no segundo modelo havia sido discretamente superestimado, devido à provável 
# correlação positiva entre estes duas VIs. Finalmente, a capacidade explicativa
# do modelo C é maior que a do modelo A e igual a do modelo B, o que aponta que
# no modelo A o efeito da VI analisada era, na verdade, o efeito compartilhado
# por renda e % diploma. 


---
  
  
  # Questão 4:

### 4.1

## letra a)

# Carregar base:
require(ggplot2)  
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_09")
WordRecall <- read_tsv("wordrecall.txt")
head(WordRecall)

# Modelo linear:
reg <- lm(prop ~ time, data = WordRecall)

# Analisar regressão:
summary(reg)

# Plotar:
ggplot(data = WordRecall, aes(y = prop, x = time))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid <- resid(reg)
ggplot(data = WordRecall, aes(y = resid, x = time)) +
         geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg, which = 1:4)


# Neste modelo linear, 0.57 da variação da VD "proporção de itens lembrados" (prop), 
# deve-se ao efeito da VI "tempo" (time). O ^beta = -5.57 aponta que para o
# aumento de 1 unidade na VI time, haverá uma diminuição de 5.57 unidades 
# na VD prop. O erro médio quadrártico (RSE) é de 0.15. Ao plotar a regressão é 
# possível perceber que alguns pressupostos foram violados; há uma relação não 
# linear entre as variáveis, é possível identificar que há resíduos com valores
# destoantes, além de não estarem distribuídos aleatoriamente ao longo da linha
# da média no gráfico da análise residual. A distribuição dos resíduos parece
# aproximar-se de uma normal. 
--------------------------------------------------------------------------------
# Modelo level-log:

# Transformar dados e criar nova base:
require(dplyr)
lntime <- log(WordRecall$time)  
WordRecall2 <- mutate(WordRecall, new = lntime)

# Regressão
reg.log <- lm(prop ~ lntime, data = WordRecall2)
summary(reg.log)

# Plotar:
ggplot(data = WordRecall2, aes(y = prop, x = lntime))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid2 <- resid(reg.log)
ggplot(data = WordRecall2, aes(y = resid2, x = lntime)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg.log, which = 1:4)

# Neste segundo modelo, a capacidade explicativa aumentou; o valor do R^2 aponta
# que 0.99 da variação na VD deve-se ao efeito da VI em questão. Este modelo 
# também apresentou valores mais significativos para ^alfa e ^beta (menor p-valor).
# O pressuposto da linearidade passa a ser atendido quando a variável independente
# é transformada em log. 

## letra b)

# Carregar base:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_09")
ShortLeaf <- read_tsv("shortleaf.txt")
head(ShortLeaf)

# Modelo Linear:

# Regressão:
reg2 <- lm(Vol ~ Diam, data = ShortLeaf)
summary(reg2)

# Plotar:
ggplot(data = ShortLeaf, aes(y = Vol, x = Diam))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid3 <- resid(reg2)
ggplot(data = ShortLeaf, aes(y = resid3, x = Diam)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg2, which = 1:4)

# A VI é responsável por 0.89 da variação na VD. Neste caso, a distribuição não 
# aproxima-se de uma normal, há uma relação não linear entre as variáveis e o 
# erro padrão dos resíduos não apresentam a mesma variância (o RSE apresenta um 
# valor relativamente alto, de 9.87). O modelo adequado para este caso é o 
# log-log, que transforma em log tanto X, quanto Y. 
--------------------------------------------------------------------------------
# Modelo log-log:
  
# Transformar dados:
ShortLeaf2 <- log(ShortLeaf)  

# Regressão:
reg.log2 <- lm(Vol ~ Diam, data = ShortLeaf2)
summary(reg.log2)

# Plotar:
ggplot(data = ShortLeaf2, aes(y = Vol, x = Diam))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid4 <- resid(reg.log2)
ggplot(data = ShortLeaf, aes(y = resid4, x = Diam)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg.log2, which = 1:4)

# Neste modelo log-log, a VI é reponsável por 0.97 da variação na VD. Ao plotar
# a regressão e a análise dos resíduos é possível observar uma relação linear entre
# as variáveis e os resíduos parecem estar distribuídos aleatoriamente (se não
# existe um padrão, é possível assumir que as variâncias serão aproximadamente
# iguais). Também, a distribuição dos resíduos parece aproximar-se mais de uma
# normal, quando comparada à distribuição do modelo anterior.

## letra c)

# Modelo linear:

# Carregar base:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_09")
MammGest <- read_table("mammgest.txt")
head(MammGest)

# Regressão:
reg3 <- lm(Gestation ~ Birthwgt, data = MammGest)
summary(reg3)

# Plotar:
ggplot(data = MammGest, aes(y = Gestation, x = Birthwgt))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid5 <- resid(reg3)
ggplot(data = MammGest, aes(y = resid5, x = Birthwgt)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg3, which = 1:4)

# Ao analisar os gráficos, é possível perceber que neste modelo há uma relação
# linear entre as variáveis e que 0.84 da variação da VD, deve-se ao efeito
# da VI. Também é possível observar que a distribuição dos resíduos aproxima-se
# de uma distribuição normal; no entanto, a variância dos resíduos não é igual.
# Para desenvolver um modelo melhor ajustado, é necessário transformar Y em log.
--------------------------------------------------------------------------------
# Modelo log-level:
  
# Transformar dados e criar nova base:
  require(dplyr)
lngest <- log(MammGest$Gestation)  
MammGest2 <- mutate(MammGest, lngest = lngest)

# Regressão
reg.log3 <- lm(lngest ~ Birthwgt, data = MammGest2)
summary(reg.log3)

# Plotar:
ggplot(data = MammGest2, aes(y = lngest, x = Birthwgt))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid6 <- resid(reg.log3)
ggplot(data = MammGest2, aes(y = resid6, x = Birthwgt)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg.log3, which = 1:4)

# Neste modelo, apesar de o R^2 apresentar menor valor do aquele encontrado no
# modelo linear, o pressuposto da igual variância dos resíduos é atendido,
# portanto, para este caso, o modelo log-level se mostra mais adequado. 
 

### 4.2

## letra a)

# Os modelos polinomiais buscam representar relações não-lineares entre 
# variáveis. Para estes modelos, são adicionadas novas variáveis "dummys" que
# adicionam uma "nova dimensão" (ou seja, são necessariamente modelos
# multivariados) à representação da regressão e faz com que exista um melhor 
# ajuste da linha aos dados. No modelo polinomial a variável adicionada é um
# termo polinomial da própria variável independente. 


## letra b)

# Carregar base:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_09")
BlueGills <- read_tsv("bluegills.txt")
head(BlueGills)

# Modelo Linear: 
# Regressão:
reg4 <- lm(length ~ age, data = BlueGills)
summary(reg4)

# Plotar:
ggplot(data = BlueGills, aes(y = length, x = age))+
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Analisar resíduos:
# Opção 1:
resid7 <- resid(reg4)
ggplot(data = BlueGills, aes(y = resid7, x = age)) +
  geom_point()+
  geom_smooth(method = "lm", color = "lightblue", se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg4, which = 1:4)

# Este modelo, apesar de apresentar um R^2 de 0.73, não parece ser o mais 
# adequado, já que os os resíduos estão muito dispersos e sugerem uma relação
# não linear entre as variáveis, como é possível verificar pelo valor de 12.51 
# do RSE e pela análise dos gráficos residuais.  
--------------------------------------------------------------------------------
# Modelo quadrático:

# Transformar dados (adicionar termo quadrático): 
BlueGills2 <- mutate(BlueGills, age2 = BlueGills$age^2)

# Regressão quadrática:
reg5 <- lm(length ~ age + age2 , data = BlueGills2)
summary(reg5)

# Plotar:
ggplot(data = BlueGills2, aes(y = length, x = age))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ poly(x,2), color = "lightblue",
              se = F)

# Analisar resíduos:
# Opção 1:
resid8 <- resid(reg5)
ggplot(data = BlueGills2, aes(y = resid8, x = age)) +
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ poly(x,2), color = "lightblue",
              se = F)

# Opção 2:
par(mfrow=c(2,2))
plot(reg5, which = 1:4)  
  
  
# Este modelo apresenta um melhor ajuste, com o R^2 = 0.80 e a análise dos
# gráficos dos resíduos permite observar que estes estão melhor distribuídos em
# relação à sua média.

## letra c)

# É possível observar que a relação entre a idade e o comprimento dos peixes da 
# espécie Bluegill é positiva e não linear. Ao analisar o valor do R^2, é 
# possível observar que aproximadamente 80% da variação no comprimentos destes
# peixes é explicada pela idade. 
  











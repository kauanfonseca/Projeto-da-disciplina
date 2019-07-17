# Aula 06: Análise explanatória de dados

data("anscombe")

#Funções para checar os dados

dim(anscombe)
head(anscombe)
class(anscombe)
str(anscombe)
names(anscombe)

#Selecionando colunas dos dados

#Médias por cada coluna com o conjunto "x"

mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

#Função "apply"

apply(anscombe[,1:4],2, mean) #aplica uam função a todas as linhas de objeto

apply(anscombe[,5:8], 2, mean)  #aplica uam função a todas as linhas de objeto

# Descrição estatística dos dados

#Variância dos dados
# Utiliza-se "var" na função apply

apply(anscombe, 2, var) #aplica a função "var" a todas as linhas do objeto

#Ententendo a correlação e coeficiente de regressão dos conjuntos x e y

# correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

summary(mlist[[1]])
library(ggplot2)

par(mfrow=c(2,2), las=1, bty="l") #abre uma janela gráfica com duas linhas e duas colunas

plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])
par(mfrow=c(1,1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

#testando ggplot2###
ggplot(anscombe, aes(x=anscombe$x1, y=anscombe$y1)) +
  geom_point()+
  stat_smooth(method="lm", col= "red")
par(mfrow=c(2,2), las=1,bty="l")
ggplot(anscombe, aes(x=anscombe$x1, y=anscombe$y1)) + geom_point()+ stat_smooth(method="lm", col= "red")
#####################

data(iris)
head(iris)
summary(iris)

table(iris$Species)

# media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data=iris, mean)

#calcular o desvio padrão

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# Executar uma sequência de tarefas utilizando a função "for"

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol=3, nrow=4)
medias
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
medias
rownames(medias) <- names(iris)[-5]
medias
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}
medias

#Estatística descritiva

#tendências central:
#média
vars <- iris [,-5]
apply(vars, 2,var)

#mediana
apply(vars,2, median)

#moda
freq_sl <-sort(table(iris$Sepal.Length), decreasing=TRUE)
freq_sl[1]

#medidas de dispersão:

#variância
apply(var,2, var)

#desvio padrão
sd01 <- apply(vars, 2, sd)

#coeficiente de variação
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

#quantis
# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

#range
# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

#IIQ (doferença entre 75q e 25q)

apply(vars, 2, IQR)

#correlação

cor(vars)

#métodos gráficos

#histograma
#para as spp de Iris
par(mfrow=c(2,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1,1))

#Número de intervalos no istog.para sépalas das spp de Iris com o argumento "break"

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1,1))

#Curva de denssidade
#densidade probabilistica
par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
par(mfrow=c(1,1))
#a curva pela função "density"
par(mfrow=c(1,2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE
par(mfrow=c(1,1))

#Blox-plot

#compreendendo o blox-plot
set.seed(2)
par(bty="n")
bp <- rnorm(1000, 0, 0.1)
boxplot(bp, yaxt="n", xlim=c(0,3), ylim=c(-0.3, 0.3))
text(x=1.82, y=min(bp), "último ponto (-1,5 x IIQ)", cex=.9)
text(x=1.72, y=quantile(bp)[2], "primeiro quartil", cex=.9)
text(x=1.72, y=median(bp), "mediana", cex=.9)
text(x=1.72, y=quantile(bp)[4], "terceiro quartil", cex=.9)
text(x=1.82, y=bp[203], "último ponto (+1,5 x IIQ)", cex=.9)
arrows(x0=0.68, x1 = 0.68, y0=quantile(bp)[2], y1=quantile(bp)[4], code=3, length=0.05)
text(x=0.54, y=median(bp), "IIQ", cex=.9)

#blox-plot iris

boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#blox-plot por spp de iris

boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Width ~ Species, data=iris)

#outliers

boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

#distribuição dos dados com a função "qqnorm" e "qqline"

par(mfrow=c(1,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"],
       main="setosa")
qqline(iris$Sepal.Length[iris$Species=="setosa"])
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"],
       main="versicolor")
qqline(iris$Sepal.Length[iris$Species=="versicolor"])
qqnorm(iris$Sepal.Length[iris$Species=="virginica"],
       main="virginica")
qqline(iris$Sepal.Length[iris$Species=="virginica"])
par(mfrow=c(1,1))

#relação entre variáveis pela "pairs"

pairs(vars)

#R GGally
install.packages("GGally")
library(GGally)
ggpairs(vars)

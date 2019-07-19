# Aula 07: Gráficos

#Dados utilizados

sal <- read.csv("https://raw.githubusercontent.com/AndreaSanchezTapia/analise_de_dados_ENBT_2019/master/aula07/data/salarios.csv")

#Dispersão
#plot com "abline" do modelo de regressão

head(sal)
summary(sal)

# criando objetos e modelos lineares para auxiliar a construção do gráfico
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)
coefm <- coef(mm)

#lendo o objeto para eu entende porque criei um:
mh
mm
coefh
coefm

# definindo os limites dos eixos no R base
limy <- c(min(sal$salario),max(sal$salario)) #adicionando os valores limites no eixo y
limx <- c(min(sal$experiencia),max(sal$experiencia)) #adicionando os valores limites no eixo x
## definindo os nomes dos eixos no R base
labx <- "Experiência (anos)" #adicioando legenda no eixo x
laby <- "Salário (R$)" #adicionando legenda no eixo y

# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") #aqui estamos usando las (colocar os valores do eixo y para a horizontal) e bty (formato do gráfico em em caixa, mas em "L") dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx) #plotando os gráficos com os objetos criados anteriormente
# linha do previsto pelo modelo
## a + b.x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2) #adicionando os coificientes na reta do gráfico; para as demais funções verificar lista do R
mtext("A", 3, adj=0, font=2) #mtext: corresponde a lengenda da imagem (e.g., Gráfico A, Gráfico B...)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)

#Queremos exportar esse gráfico. Para isso, primeiro crie um diretório /figs em seu repositório. Para exportar o gráfico vamos usar a função png, especificando a resolução e dimensões da figura. Quando criamos gráficos com a função png o que fazemos é:

#anunciar qual extensão e arquivo vamos plotar o gráfico com a função png()

#determinar a sequência de comandos que cria o gráfico

#finalizar a construção do arquivo com a função dev.off()

# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("../Data/Figs/figura01.png", res=300, width=2400, height=1200)
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()
## png
##   2

#refazendo com o ggplot2
library(ggplot2)
sal <- read.csv("https://raw.githubusercontent.com/AndreaSanchezTapia/analise_de_dados_ENBT_2019/master/aula07/data/salarios.csv")
fit1 <-lm(salario ~ experiencia+sexo([sal$sexo=="H"], [sal$sexo=="M",]]), data= sal)
summary(fit1)
coef(fit1)

ggplot(sal, aes(x="Experiência (anos)", y= "Salário(R$)", color=sexo)) + geom_point() + stat_smooth(method = "lm", col = "red")

################ terminar depois ################

#Exercício e desafio aula 7

data(iris)
head(iris)
summary(iris)

### setosa =a; versicolor=b; virginica=c ###

a<-lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="setosa",])
b <- lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="versicolor",])
c <- lm(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="virginica",])
coefa <- coef(a)
coefb <- coef(b)
coefc <- coef(c)
limy <-c(min(iris$Sepal.Length), max(iris$Sepal.Length))
limx <-c(min(iris$Petal.Width), max(iris$Petal.Width))
labx <- "Comprimento da pétala"
laby <- "Largura sépala"

#exportar gráfico
png("../Data/Figs/figura02.png", res=300, width=2400, height=1200)

par(mfrow=c(1,3), las=1, bty="l")

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="setosa",], col="green", ylim=limy, xlim=limx, ylab=laby, xlab=labx)
abline(a=coefa[1], b=coefa[2], col='green', lwd=2)
mtext("A", 3, adj=0, font=2)

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="versicolor",], col="purple", ylim=limy, xlim=limx, ylab=laby, xlab=labx)
abline(a=coefb[1], b=coefb[2], col='purple', lwd=2)
mtext("B", 3, adj=0, font=2)

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="virginica",], col="pink", ylim=limy, xlim=limx, ylab=laby, xlab=labx)
abline(a=coefc[1], b=coefc[2], col='pink', lwd=2)
mtext("C", 3, adj=0, font=2)

dev.off()


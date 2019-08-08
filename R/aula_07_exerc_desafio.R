#Aula 07: README

#Objetivo: contruir uma janela gráfica com 1 linha e três colunas com a base de dados "iris"

#Para tanto:
#1) Comprimento da pétala no eixo x e largura da sépala no eixo y, sendo cada espécie em uma janela gráfica
#2)Padronize os eixos x e y para todas as espécies
#3)Plote cada espécie com uma cor diferente
#4)Adicione a reta do modelo linear
#5)Inclua rótulos A, B e C em cada uma das janelas gráficas
#6)Crie um diretório /figs em seu repositório
#7)Salve o gráfico em png em uma boa resolução, com o tamanho dos pontos e eixos legíveis no diretório /figs

#pacotes necessários
#ggplot2
#devtools::install_github("karthik/wesanderson")

library(ggplot2)
library(wesanderson)

data("iris")
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

png("Figs/lm_exerc7.png", res=300, width=2400, height=1200)
par(mfrow=c(1,3), las=1, bty="l")

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="setosa",], col="darkgoldenrod2", ylim=limy, xlim=limx, ylab=laby, xlab=labx, pch=19, col.axis = 'black', col.lab = 'black', cex.axis = 1.2, cex.lab = 1.2)
abline(a=coefa[1], b=coefa[2], col='darkgoldenrod2', lwd=2)
mtext("A", 3, adj=0, font=2)


plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="versicolor",], col="aquamarine2", ylim=limy, xlim=limx, ylab=laby, xlab=labx, pch=19, col.axis = 'black', col.lab = 'black', cex.axis = 1.2, cex.lab = 1.2)
abline(a=coefb[1], b=coefb[2], col='aquamarine2', lwd=2)
mtext("B", 3, adj=0, font=2)

plot(Sepal.Length ~ Petal.Width, data=iris[iris$Species=="virginica",], col="coral2", ylim=limy, xlim=limx, ylab=laby, xlab=labx, pch=19, col.axis = 'black', col.lab = 'black', cex.axis = 1.2, cex.lab = 1.2)
abline(a=coefc[1], b=coefc[2], col='coral2', lwd=2)
mtext("C", 3, adj=0, font=2)

dev.off()

#Desafio
#Desafio aula 07

#calculando as médias

SL <-mean(iris$Sepal.Length)
SW <-mean(iris$Sepal.Width)
PL <-mean(iris$Petal.Length)
PW <-mean(iris$Petal.Width)

#calculando o desvio padrão

SLsd <-sd(iris$Sepal.Length)
SWsd <-sd(iris$Sepal.Width)
PLsd <-sd(iris$Petal.Length)
PWsd <-sd(iris$Petal.Width)

#concatenar as médias e désvio padrão
variaveis <-c(SL, SW, PL, PW)
desvio <-c(SLsd, SWsd, PLsd, PWsd)

#criando um data frame
df<-data.frame(name=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), media=variaveis,desvio_padrao= desvio)
df

png("Figs/desafio.png", res=300, width=2400, height=1200)
par(mfrow=c(1,3), las=1, bty="l")

#plotando com o ggplot2
ggplot(df, aes(factor(name), media)) + geom_point(shape=19, fill="black", color="paleturquoise4", size=3)+ geom_errorbar(aes(ymin=media-desvio, ymax=media+desvio), width=0.1, ) + labs(x="Variáveis", y= "Média")+ theme_classic() + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
dev.off()

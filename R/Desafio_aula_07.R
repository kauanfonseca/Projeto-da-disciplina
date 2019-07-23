#Desafio aula 07

library(ggplot2)

#carregando os dados
data("iris")

#visualizando os dados
summary(iris)
str(iris)
head(iris)

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

#plotando o gráfico em R básico
plot (x=1:4, df$media,las=1,ylim = c(-1,8), pch=19, xaxt='n', xlab ="Variáveis", xlim = c(0,6), ylab = "Médias", bty="l")
axis(1, at=1:4, labels = df$name)
arrows(x0=1:4, y0=df$media+df$desvio_padrao, y1=df$media-df$desvio_padrao, angle=90, length=0.05, code=3)

#plotando com o ggplot2
ggplot(df, aes(factor(name), media)) + geom_point(stat='identity', colour="pink", size=2)+ geom_errorbar(aes(ymin=media-desvio, ymax=media+desvio), width=0.1) + xlab("Variáveis") + theme_get()



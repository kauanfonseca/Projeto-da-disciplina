#Aula 06: exercício 05

#Objetivo: refazer o tutorial da aula 06

#pacotes necessários:
#install.packages("ggplot2")
#devtools::install_github("ropensci/plotly")
#install.packages("GGally")

#carregando pacotes

library(ggplot2)
library(plotly)
library(GGally)

arvs <- read.csv("./Data/trees.csv", sep=';', header = TRUE)
summary(arvs)
#variância

#calculando a variância:
apply(arvs, 2, var)

#calculando o desvio padrão:
apply(arvs, 2, sd)

png("figs/histo_01.png", res=300, width=2400, height=1200)
#histograma no ggplot2
g<- ggplot(arvs, aes(x=Girth))+ theme_classic() + ylab("Frequency") + geom_histogram(color="black", fill="turquoise4", binwidth = 1.8, size=1) + geom_vline(aes(xintercept=mean(Girth)), color='violetred3', linetype="dotted", size=1.5) + scale_x_continuous(limits = c(8,21)) + scale_y_continuous(limits = c(0,20)) + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
g

dev.off()

png("figs/histo_2.png", res=300, width=2400, height=1200)
h<- ggplot(arvs, aes(x=Height))+ ylab("Frequency") + theme_classic() + geom_histogram(color="black", fill="tomato3", binwidth = 1.8, size=1) + geom_vline(aes(xintercept=mean(Height)), color='turquoise4', linetype="dotted", size=1.5) + scale_x_continuous(limits = c(60, 90)) + scale_y_continuous(limits = c(0,20)) + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
h
dev.off()

png("figs/histo_3.png", res=300, width=2400, height=1200)
v<- ggplot(arvs, aes(x=Volume)) + ylab("Frequency") + theme_classic() + geom_histogram(color="black", fill="violetred3", binwidth = 3,size=1) + geom_vline(aes(xintercept=mean(Volume)), color='turquoise1', linetype="dotted", size=1.5) + scale_x_continuous(limits = c(10,80)) + scale_y_continuous(limits = c(0,5)) + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
v
dev.off()

#boxplot para cada variável

png("figs/bp_01.png", res=300, width=2400, height=1200)
#Girth
bpg<-(arvs$Girth)

girth_bp <- ggplot(arvs, aes(Girth,bpg)) + geom_boxplot(fill="lightseagreen", color="black", size=1)

girth_bp +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + theme_classic() + labs(x="Girth", y= "Frequency") + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
dev.off()

png("figs/bp_02.png", res=300, width=2400, height=1200)
#Height
bph<-(arvs$Height)
height_bp<- ggplot(arvs, aes(Height,bph)) + geom_boxplot(fill="lightsalmon2", color="black", size=1)
height_bp + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + theme_classic() + labs(x="Girth", y= "Frequency") + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
dev.off()

png("figs/bp_03.png", res=300, width=2400, height=1200)
#Volume
bpv<-(arvs$Volume)
volume_bp<- ggplot(arvs, aes(Volume,bpv)) + geom_boxplot(fill="khaki", color="black", size=1)
volume_bp + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + theme_classic() + labs(x="Girth", y= "Frequency") + theme(axis.text.x = element_text(face="bold", color="black", size=11), axis.text.y = element_text(face="bold", color="black", size=11)) + theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))
dev.off()


#identificar os outliers

outliers<- boxplot(arvs$Girth, plot=FALSE)
outliers

png("figs/m_cor.png", res=300, width=2400, height=1200)
#Correlação
correlacao <- cor(arvs)
correlacao
write.csv(correlacao,"./results/correlação_exerc5.csv")
ggcorr(arvs[,1:3], label = TRUE)
dev.off()


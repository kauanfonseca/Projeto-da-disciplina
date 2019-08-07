#Exercício 04

#Objetivo: selecionar as colunas de espécies,longitude e latitude da tabela; salvar o arquivo com estes dados.


algas <- read.csv("./data/0012594-190621201848488.csv", sep="\t", quote = '/"', dec=".", fill=TRUE, comment.char = "", header=TRUE)

head(algas)
dim(algas)

#separar as colunas
spelatelong <- algas[,c(10,17,18)]
head(spelatelong)

#salvar o arquivo modificado
write.csv(spelatelong,'./results/exerc4_feito.csv')

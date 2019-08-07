#Exercício 03

#Objetivos: salvar os dados em apenas uma aba do excel e organizá-los, salvar no formato csv e calcular o número de indivíduos por parcela de 1 hectare na Ilha de Barro Colorado.

exerc3 <- read.csv("./data/exercicio_03.csv", sep = ';', header = TRUE)
exerc3

#somar linhas e colunas:
colSums(exerc3[c(2:51)])


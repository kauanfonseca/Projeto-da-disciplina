#Exercício 02

#Objetivo: organizar os dados e salva-los em formato "csv". Também é necessário calcular a média de uptake para Quebec chilled.

exerc2 <-read.csv("./data/exercicio_02.csv", sep = ";", header = TRUE)
exerc2

#separar colunas e linhas de interesse
exerc2[22:42, c(1,2,4)]

#média uptake para Quebec/chilled
mean(exerc2[22:42, c(4)])



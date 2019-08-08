## tutorial da aula 08
#Tutorial da aula de SIG > Mapas!

library(rgdal)
library(raster)

#Importar um shape e plotar:
westeros <- readOGR("./Data/Westeros_Essos_shp/GoTRelease/political.shp", encoding = "UTF-8")

plot(westeros, las = 1, axes = T)
abline(h = 0, lty = 2, col = "tomato")

#checar colunas dos dados:
names(westeros)

#dados de coluna específica:
westeros$ClaimedBy

#selecionar uma região, criando um objeto só com a coluna stark
stark <- westeros[westeros$ClaimedBy == "Stark",]
plot(stark, axes = T, las = 1)

#criando um buffer:
pontos <- spsample(stark, 10, 'random')
plot(stark, axes = T)
points(pontos, pch = "+", col = "tomato", cex = 1.5)

#para criar o buffer , deverá utilizar o pacote raster
pontos.buffer <- buffer(pontos, width = 200000, dissolve = TRUE)

plot(stark, axes = T)
plot(pontos.buffer, add = T, col = "cadetblue1")
points(pontos, col = 'red', pch = 16)

#criar um buffer de polígono:
stark.buffer <- buffer(stark, width = 2, dissolve = TRUE)
plot(stark.buffer, col = "grey80", axes = T)
plot(stark, add = T, col = "lightblue")

#Incluindo atributos em um objeto Spatial
westeros
westeros$regiao <- c(rep(1:3, each = 4))
westeros


#unindo polígonos:
westeros_contorno = aggregate(westeros)
plot(westeros_contorno, axes = T)
plot(westeros, axes = T, col = terrain.colors(12))

#unir polígonos por região:
new_westeros = aggregate(westeros, by = "regiao")
plot(new_westeros, axes = T, col = terrain.colors(4))


#exportando um shape:
writeOGR(
  westeros_contorno, #nome do objeto a ser salvo
  dsn = "./Results", #diretorio a serem salvos os resultados
  layer = "westeros_contorno", #nome do arquivo
  driver = "ESRI Shapefile" #formato pretendido para exportação
)

#deu erro ao utiliar essa função, então nós concertamos fazendo uma correção:
westeros_contorno$id<-1

#e depois voltamos a exportar com a mesma função anterior:
writeOGR(
  westeros_contorno, #nome do objeto a ser salvo
  dsn = "./Results", #diretorio a serem salvos os resultados
  layer = "westeros_contorno", #nome do arquivo
  driver = "ESRI Shapefile" #formato pretendido para exportação
)

#criando raster a partir de um shape:
westeros_raster <- raster(westeros_contorno, res = 0.08)
westeros_raster <- rasterize(westeros_contorno, westeros_raster) #deixando com o mesmo extent
plot(westeros_raster)

#importando um raster:
var1 <- raster("./Data/Westeros_Essos_shp/vars/var_1.tif")
var1
plot(var1)

#quando são múltiplos rasters:
lista <- list.files("./Data/Westeros_Essos_shp/vars", pattern = "tif$", full.names = T)
vars <- stack(lista)
plot(vars)
vars <- stack("./Data/Westeros_Essos_shp/vars.tif")


#slvando o raster no arquivo:
writeRaster(var1, "output.tif", overwrite=TRUE)

##media do gráfico
media <- mean(vars)
plot(media)

## modificar o raster
westeros <- readOGR("./Data/Westeros_Essos_shp/GoTRelease/political.shp", encoding = "UTF-8")
stark <- westeros[westeros$ClaimedBy == "Stark",]
stark

plot(westeros, axes = T, las = 1)
plot(stark, add = T, col = "tomato")

#agora vamos recortar esse plot. Foi utilizado a função crop:
plot(var1)
plot(westeros, add = T)
var1_croped <- crop(var1, stark)
var1_croped
plot(var1_croped)
plot(stark, add = T)

#Com a função mask podemos recortar por uma mascara.
var1_masked = mask(var1, stark)
var1_masked
plot(var1_masked)
plot(stark, add = T)

#combinando as funçãos crop e mask
var1.masked2 = mask(crop(var1,stark), stark)
var1.masked2
plot(var1.masked2)
plot(stark, add = T)

##alterando a resolução do raster
var1.aggregated = aggregate(var1, fact = 5, fun = "mean")
var1.aggregated
plot(var1.aggregated)

#Fim

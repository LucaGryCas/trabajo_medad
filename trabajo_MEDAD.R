load("C:/Users/USUARIO/clase/2º/1ªQ/MEDAD/trabajo MEDAD/Spain.RData")
datos <- Spain[-c(2, 6)]

# ACP 

str(datos)
summary(datos)

library(psych)
datos.pca.varcov <- prcomp(datos)
summary(datos.pca.varcov)

datos.pca.varcov$sdev^2 # varianza por componente

screeplot(datos.pca.varcov, type = "line")

# Factores de carga
datos.pca.varcov$rotation
barplot(datos.pca.varcov$rotation, beside = TRUE) # representación de los ceficientes

# Representación grafica
library(ggbiplot)
ggbiplot(datos.pca.varcov, labels = row.names(datos))


  # estandarizamos los datos
datos.scale <- scale(datos, center = TRUE, scale = TRUE)
summary(datos.scale)

datos.scale.pca.varcov <- prcomp(datos.scale)
summary(datos.scale.pca.varcov)
datos.scale.pca.varcov$sdev^2 # varianza por componente

screeplot(datos.scale.pca.varcov, type = "line")

# factores de carga
datos.scale.pca.varcov$rotation

# Representación grafica
ggbiplot(datos.scale.pca.varcov, labels = row.names(datos))

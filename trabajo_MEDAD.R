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



# MDS en R (manual)

library(philentropy)
dist.dat <- as.matrix(dist(Spain))

n <- dim(dist.dat)[1]

A <- (-1/2)*dist.dat^2 # Matriz A

# Matriz de centrado
H <- diag(rep((n-1)/n,length.out=n))  # En la diagonal 6/7
H[which(H==0)] <- -1/n   # En las otras posiciones, -1/7
H

# Matriz B = HAH
B <- H %*% A %*% H; B

  # 7: Coordenadas de los puntos obtenidos
lambda <- eigen(B)$values # autovalores de B
autovec <- eigen(B)$vectors #autovectores de B

vec12 <- autovec[,1:2]
lambda_12 <- diag(lambda[1:2])

result <- vec12 %*% sqrt(lambda_12); result

d.names <- row.names(Spain)

  ## 8: Representación grafica 
plot(result, pch=19, main="Resultado del MDS clásico", xlab="x",ylab="y",
     xlim=c(-10000,10000),ylim=c(-80,50))
text(result-c(5,7), labels=d.names,cex=0.9, font=2)

  ## 11: MDS con variables
# dist.var <- as.matrix(cor(Spain))
dist.var <- as.matrix(prcomp(scale(Spain, center = TRUE, scale = TRUE))$rotation)

d.names <- colnames(Spain)

n <- dim(dist.var)[1]

fit <- cmdscale(dist.var, eig = TRUE, k = 2); fit

x <- fit$points[,1]
y <- fit$points[,2]

# Representación grafica
plot(x,y, pch=19, main="Resultado del MDS sobre las Variables", xlab="x",ylab="y",
     xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
text(result-c(0.1,0.1), labels=d.names, font=2)

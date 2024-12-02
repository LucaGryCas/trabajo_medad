# Scritp de R 
## ??lvaro L??pez P??rez
## Luca Grygar Casas


# Cargamos los datos

load("Spain.RData") 
datos <- Spain 
attach(datos) 

# 1) Ejercicio

## Estudio univariante y multivariante

head(datos) 
str(datos) 
cat("PIB: ",range(pib), "\n")
library(dplyr)
ord <- datos %>% arrange(desc(pib))

### Este ser??a el top 5:
h.datos <- head(ord,n=5)
h.datos

### Este ser??a el top 5 colista:
t.datos <- tail(ord,n=5)
t.datos

h.mean <- colMeans(h.datos)[c(-6,-7)]
t.mean <- colMeans(t.datos)[c(-6,-7)]
h.mean
t.mean


## Estudio de las correlaciones
datos.cor <- cor(datos)
cat("Correlaciones a destacar: ","\n")
cat("Emigraci??n/Inmigraci??n: ",datos.cor[2,1],"\n")
cat("Paro/empleo: ", datos.cor[4,5], "\n")
cat("IPC/PIB: ", datos.cor[7,6], "\n")

library(psych) 
pairs.panels(datos, smooth = TRUE, density=TRUE, digits = 2, ellipses=TRUE, 
             method="pearson", pch = 20, lm=TRUE, cor=TRUE)


# 2/3/4

### Nosotros para resolver el problema utilizaremos la distancia eucl??dea
datos.scale <- scale(datos,center=TRUE,scale=TRUE) 

#### datos escalados (pib con un rango enorme)
d.scale<- dist(datos.scale) 

#### datos sin escalar
d<- dist(datos)

#### distancia de mahalanobis:
library(StatMatch) 
d.mahalanobis <- mahalanobis.dist(datos)


# 5/6/7/8/9/10

# MDS en R (manual)

## Matriz A:
library(philentropy) 
dist.dat <- as.matrix(dist(datos.scale))
n <- dim(dist.dat)[1]
A <- (-1/2)*dist.dat^2 
cat("Matriz A (4 primeras columnas): \n"); A[,1:4]


## Matriz de centrado
H <- as.matrix(diag(rep((n-1)/n,length.out=n))) # En la diagonal 6/7 
H[which(H==0)] <- -1/n # En las otras posiciones, -1/7 
cat("Matriz H (4 primeras columnas): \n"); H[, 1:4]

## Matriz B = HAH
cat("Matriz B (4 primeras columnas): \n")
(B <- H %*% A %*% H)[,1:4]
cat("\n Autovalores de B: \n")
(lambda <- eigen(B)$values) 
vec12 <- autovec[,1:2] 
lambda_12 <- diag(lambda[1:2])
cat("\n Puntos elegidos: \n")
(result <- vec12 %*% sqrt(lambda_12))
d.names <- row.names(Spain)

### Representaci??n Gr??fica
plot(result, pch=19, main="MDS datos escalados, sin funci??n de R ", xlab="x",
     ylab="y", xlim=c(-3,3),ylim=c(-4,3))
text(result-c(0, 0.35), labels=d.names,cex=0.5, font=2)

# MDS funci??n R

## Datos escalados
datos.scale <- scale(datos,center=TRUE,scale=TRUE) 
d <- as.matrix(dist(datos.scale))
cat("Puntos elegido:; \n")
(fit <- cmdscale(d, eig=TRUE, k=2))$points # k es el n??mero de dimensiones 

### Representaci??n Gr??fica
x <- fit$points[,1]; y <- fit$points[,2]
plot(x,y, pch=19, main="MDS datos escalados, con funci??n R ", xlab="x",ylab="y", xlim=c(-3,3),ylim=c(-4,3))
text(x,y-0.35, labels=d.names,cex=0.5, font=2)

## Datos sin escalar
d <- as.matrix(dist(datos))

lab <- row.names(datos)
cat("Puntos elegidos: \n")
(fit <- cmdscale(d, eig=TRUE, k=2))$points # k es el n??mero de dimensiones 
x <- fit$points[,1]; y <- fit$points[,2]
x.max <- max(x); x.min <- min(x)
y.max <- max(y); y.min <- min(y)

### Representaci??n Gr??fica
plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2", main="MDS con las variables sin escalar", 
     pch=19, xlim=c(x.min-400,x.max+400),ylim=c(y.min-10,y.max+10)) 
text(x, y-0.5, labels = d.names, cex=0.6, font=2)


# 11

## MDS con las variables
dist.var <- as.matrix(prcomp(scale(Spain, center = TRUE, scale = TRUE))$rotation)
v.names <- colnames(Spain)
n <- dim(dist.var)[1]
cat("Puntos elegidos: \n")
fit <- cmdscale(dist.var, eig = TRUE, k = 2); fit$points
x <- fit$points[,1]; y <- fit$points[,2]

## Representaci??n gr??fica
plot(x,y, pch=19, main="MDS sobre las Variables", xlab="x",ylab="y", xlim=c(-0.5,0.5),ylim=c(-0.5,0.5)) 
text(x,y-0.04, labels=v.names,cex = 0.7, font=2)
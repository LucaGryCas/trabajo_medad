#cargamos los datos
load("C:/Users/lucag/Downloads/Spain.RData")
datos <- Spain
attach(datos)
####
#1) Ejercicio
####

####
#Estudio univariante y multivariante
####
head(datos)
str(datos)

#Tenemos un conjunto de datos numéricos que evalúa distintas características
#de las comunidades autónomas españolas


range(pib) #podemos ver que hay una diferencia enorme entre comunidades
range(ipc)


#Estudio de las correlaciones
cor(datos)
library(psych)
windows()
pairs.panels(datos, smooth = TRUE, density=TRUE, digits = 2, 
             ellipses=TRUE, method="pearson", pch = 20, 
             lm=TRUE, cor=TRUE)
#continuar...
#####
#2/3/4
#####

datos.scale <- scale(datos,center=TRUE,scale=TRUE)
d.scale<- dist(datos.scale) #con los datos escalados (pib con un rango enorme)
d<- dist(datos) #con los datos sin escalar
#nosotros para resolver el problema utilizaremos la distancia euclídea
library(StatMatch)
d.mahalanobis <- mahalanobis.dist(datos) #también puede ser lógico utilizar la distancia
#de mahalanobis

#Si utilizaramos los datos sin escalar, las distancias dependerían de las 
#unidades ,por ejemplo, si nos diese por medir la tasa de inmigración por 
#proporción de inmigrantes por 10.000 personas tendría menos relevancia
#y si fuera entre 100 más, ya que esa variable tendría mayor valor numérico


########
#5/6/7/8/9/10
########

# MDS en R (manual)
library(philentropy)
dist.dat <- as.matrix(dist(datos.scale))

n <- dim(dist.dat)[1]

A <- (-1/2)*dist.dat^2 # Matriz A

# Matriz de centrado
H <- diag(rep((n-1)/n,length.out=n))  # En la diagonal 6/7
H[which(H==0)] <- -1/n   # En las otras posiciones, -1/7
H

# Matriz B = HAH
B <- H %*% A %*% H; B

lambda <- eigen(B)$values # autovalores de B
autovec <- eigen(B)$vectors #autovectores de B

vec12 <- autovec[,1:2]
lambda_12 <- diag(lambda[1:2])

result <- vec12 %*% sqrt(lambda_12); result

d.names <- row.names(Spain)

windows()
par(mfrow = c(1,2))
plot(result, pch=19, main="Resultado del MDS clásico", xlab="x",ylab="y",
     xlim=c(-3,3),ylim=c(-3,3))
text(x,y-0.035, labels=d.names,cex=0.7, font=2)

#MDS función R

#podemos separar los datos en dos tipos, demografía y economía de la comunidad
#por lo tanto tendría sentido representar las distancias en dos dimensiones

lab <- row.names(datos)

datos.scale <- scale(datos,center=TRUE,scale=TRUE)
#d <- as.matrix(dist(datos)) #ejercicio con los datos sin escalar
d <- as.matrix(dist(datos.scale))

fit <- cmdscale(d, eig=TRUE, k=2) # k es el número de dimensiones
fit 
x <- fit$points[,1]
y <- fit$points[,2]


plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2",
     main="Resultado del MDS clásico", pch=19, xlim=c(-3,3),ylim=c(-3,3))
text(x, y-0.035, labels = lab, cex=0.7, font=2)  

######
#11
######

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
text(x+0.06,y-0.035, labels=d.names, font=2)







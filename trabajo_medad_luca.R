load("C:/Users/lucag/Downloads/Spain.RData")

datos <- Spain
attach(datos)
####
#Estudio univariante y multivariante
####
summary(datos)
head(datos)
mean(prop.inm)
sd(prop.inm)
cor(datos)

#Estudio de las correlaciones

library(psych)
windows()
pairs.panels(datos, smooth = TRUE, density=TRUE, digits = 2, 
             ellipses=TRUE, method="pearson", pch = 20, 
             lm=TRUE, cor=TRUE)

lab <- row.names(datos)
help(dist)
########
#6
########


#podemos separar los datos en dos tipos, demografía y economía de la comunidad
#por lo tanto tendría sentido representar las distancias en dos dimensiones


datos.scale <- scale(datos,center=TRUE,scale=TRUE)
#d <- as.matrix(dist(datos)) #ejercicio con los datos sin escalar
d <- as.matrix(dist(datos.scale))
fit <- cmdscale(d, eig=TRUE, k=2) # k es el número de dimensiones
fit 
x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2",
     main="Resultado del MDS clásico", pch=19, xlim=c(-100,100),ylim=c(-100,100))
text(x-0.2, y+0.3, labels = lab, cex=0.9, font=2)  # Restamos 5 para que 
# no quede la etiqueta encima de los puntos


# -------------------------- TALLER 2 ----------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Curso de Estadística I
# Facultad de Ingeniería
# Universidad del Magdalena
# Grupo 1


# ---------------------------------------------------------------------------------------------

# *Información del grupo*

# Grupo asignado: 5   (Digite aquí el número de su grupo)

# Integrantes (Apellidos y Nombres - Código)

# Integrante 1: Jimenez Nuñez Edgardo David (2019214009)
# Integrante 2: Jimenez Nuñez Edgardo Luis  (2020114020)
# Integrante 3: Avila Pineda Cristian Jair  (2018214027)


# ---------------------------------------------------------------------------------------------
# *Desarrollo de los ejercicios*
# ---------------------------------------------------------------------------------------------


######################################################################

# **Primer ejercicio: 1  (Digite aquí el número del ejercicio)

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "X": 

# Asignación de datos a la variable
x <- c(23,60,79,32,57,74,52,70,82,
       36,80,77,81,95,41,65,92,85,
       55,76,52,10,64,75,78,25,80,
       98,81,67,41,71,83,54,64,72,
       88,62,74,43,60,78,89,76,84,
       48,84,90,15,79,34,67,17,82,
       69,74,63,80,85,61)

# inciso a) Medidas de tendencia central
#Media
mean(x)
#Mediana
median(x)
#Moda
library(modeest)
mfv(x)
# inciso b) Medidas de dispersión 
#Varianza
var(x)
#Desviacion estandar
sd(x)
#Coeficiente de variacion
sd(x)/mean(x)
library(FinCal)
coefficient.variation(sd=sd(x), avg = mean(x))
#Coeficiente de asimetria
library(psych)
skew(x)
skewness(x)
kurtosi(x)

#¿Los datos son homogneos?R// Los datos son homogeneos puesto el coeficiente es debajo de 0.5(coeficiente de variacion)

#¿Los datos tiene un sesgo positivo? R//No, los datos tienen un sesgo negativo de acuerdo al coeficiente de asimetria que dio -0.9(skewness)

#¿La distribucion de los datos es leptocurtica,platicurtica o msocurtica? R// La distribucion de los datos es platicurtica puesto existe una baja concentracion de los valores en torno a su media menor a 3(kurtosi)

# inciso c) Medidas de localización 
quantile(x)

# inciso d) Histograma
hist(x, main = "Calificaciones examen final de estadistica",xlab = "Calificaciones",ylab = "Frecuencia")
abline(v=mean(x), lwd = 2, lty=3, col="darkblue")
abline(v=median(x), lwd = 2, lty=3, col="red")
abline(v=mfv(x), lwd = 2, lty=3, col="yellow")
abline(v=var(x), lwd = 2, lty=3, col="green")
abline(v=sd(x), lwd = 2, lty=3, col="orange")
abline(v=(sd(x)/mean(x)), lwd = 2, lty=3, col="gray")
abline(v=skew(x), lwd = 2, lty=3, col="pink")
abline(v=kurtosi(x), lwd = 2, lty=3, col="purple")


# inciso e) Diagrama de caja y bigotes
boxplot(x,
        main = "Calificaciones de examenes finales de estadistica",
        col = "pink",
        border = "purple")
#Si hay observaciones atipicas que se representan con un punto en el extremo del diagrama

# inciso f) Conclusiones
#En conclusion pudimos observar que el promedio de las calificaciones fue de 65.48
#con un valor medio de 71.5 y con una calificacion mas recurrente de 74 y 80.
#Los datos son homogeneos con una bastante cercania a la tendencia central. 


######################################################################

# **Segundo ejercicio: 2  (Digite aquí el número del ejercicio)

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "y": 

# Asignación de datos a la variable
y <- c(17,20,10,9,23,13,12,19,18,24,
       12,14,6,9,13,6,7,10,13,7,
       16,18,8,3,3,32,9,7,10,11,
       13,7,18,7,10,4,27,19,16,8,
       7,10,5,14,15,10,9,6,7,15)

# inciso a) Medidas de tendencia central
#Media
mean(y)
#Mediana
median(y)
#Moda
library(modeest)
mfv(y)

# inciso b) Medidas de dispersión 
#Varianza
var(y)
#Desviacion estandar
sd(y)
#Coeficiente de variacion
sd(y)/mean(y)
library(FinCal)
coefficient.variation(sd=sd(y), avg = mean(y))
#Coeficiente de asimetria
library(psych)
skew(y)
kurtosi(y)

#¿Lo datos son homogeneos? R//Los datos son heterogeneos puesto el coeficiente es superior de 0.5(coeficiente de variacion)
#¿Los datos tienen un sesgo positivo? R//si, los datos tienen un sesgo positivo de acuerdo al coeficiente de asimetria(skewness), como el sesgo es positivo encontramos una mayor concentracion de datos a la izquierda.
#¿La distribucion de los datos es leptocurtica,platicurtica o mesocurtica? R// La distribucion es platicurtica porque existe una baja concentracion de los valores en torno a su media(coeficiente de curtosisi es menor a 3)

# inciso c) Medidas de localización 
quantile(y)
# inciso d) Histograma
hist(y, main = "Duracion de la vida de las moscas")
abline(v=mean(y), lwd = 2, lty=3, col="darkblue")
abline(v=median(y), lwd = 2, lty=3, col="red")
abline(v=mfv(y), lwd = 2, lty=3, col="yellow")
abline(v=var(y), lwd = 2, lty=3, col="green")
abline(v=sd(y), lwd = 2, lty=3, col="orange")
abline(v=(sd(y)/mean(x)), lwd = 2, lty=3, col="gray")
abline(v=skew(y), lwd = 2, lty=3, col="pink")
abline(v=kurtosi(y), lwd = 2, lty=3, col="purple")

# inciso e) Diagrama de caja y bigotes
boxplot(y,
        main = "Duracion de la vida de las moscas",
        col = "pink",
        border = "purple")

#Si hay observaciones atipicas dentro del diagrama de caja y bigotes

# inciso f) Conclusiones
#En conclusion pudimos observar que el promedio de vida de las moscas fue de 12.12 segundos
#con un valor medio de 10 segundos y con duracion mas recurrente de 7 segundos.
#Ademas pudimos observar gracias a la varianza y a la desviacion estandar que los datos estuvieron menos dispersos y alejados de la tendencia central
#Comparando con los datos, la varianza elevada significa que los datos están más dispersos. Mientras que un valor de la varianza bajo indica que los valores están por lo general más próximos a la media.

######################################################################

# **Tercer ejercicio: 22 (Digite aquí el número del ejercicio)

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "A" y "B": 

# Asignación de datos a la variable
A <- c(939,976,1025,1034,1015,1015,1022,815,
       1027,1021,878,949,1019,939,840,916)
B <- c(1025,938,1015,983,843,1053,1038,938,
       1029,885,966,950,1034,895,969,954)
# inciso a) Medidas de tendencia central
#Media
mean(A)
#Mediana
median(A)
#Moda
library(modeest)
mfv(A)

#Media
mean(B)
#Mediana
median(B)
#Moda
library(modeest)
mfv(B)

# inciso b) Medidas de dispersión 
#Varianza
var(A)
#Desviacion estandar
sd(A)
#Coeficiente de variacion
sd(A)/mean(A)
library(FinCal)
coefficient.variation(sd=sd(A), avg = mean(A))
#Coeficiente de asimetria
library(psych)
skew(A)
skewness(A)
kurtosi(A)

#Varianza
var(B)
#Desviacion estandar
sd(B)
#Coeficiente de variacion
sd(B)/mean(B)
library(FinCal)
coefficient.variation(sd=sd(B), avg = mean(B))
#Coeficiente de asimetria
library(psych)
skew(B)
skewness(B)
kurtosi(B)

#Dato A
#¿Lo datos son homogeneos? R//Los datos son homogeneos puesto el coeficiente es inferior de 0.5(coeficiente de variacion)
#¿Los datos tienen un sesgo positivo? R//no, los datos tienen un sesgo negativo de acuerdo al coeficiente de asimetria(skewness), como el sesgo es negativo encontramos una mayor concentracion de datos a la derecha
#¿La distribucion de los datos es leptocurtica,platicurtica o mesocurtica? R// La distribucion es platicurtica porque existe una baja concentracion de los valores en torno a su media(coeficiente de curtosisi es menor a 3)




#Dato B
#¿Lo datos son homogeneos? R//Los datos son homogeneos puesto el coeficiente es inferior de 0.5 (coeficiente de variacion)
#¿Los datos tienen un sesgo positivo? R//no, los datos tienen un sesgo Negativo de acuerdo al coeficiente de asimetria(skewness), como el sesgo es negativo encontramos una mayor concentracion de datos a la derecha
#¿La distribucion de los datos es leptocurtica,platicurtica o mesocurtica? R// La distribucion es platicurtica porque existe una baja concentracion de los valores en torno a su media(coeficiente de curtosisi es menor a 3)

# inciso c) Medidas de localización 
quantile(A)
quantile(B)

# inciso d) Histograma
hist(A, main = "Resistencia de barras de polimeros A")
abline(v=mean(A), lwd = 2, lty=3, col="darkblue")
abline(v=median(A), lwd = 2, lty=3, col="red")
abline(v=mfv(A), lwd = 2, lty=3, col="yellow")
abline(v=var(A), lwd = 2, lty=3, col="green")
abline(v=sd(A), lwd = 2, lty=3, col="orange")
abline(v=(sd(A)/mean(A)), lwd = 2, lty=3, col="gray")
abline(v=skew(A), lwd = 2, lty=3, col="pink")
abline(v=kurtosi(A), lwd = 2, lty=3, col="purple")

hist(B, main = "Resistencia de barras de polimeros B")
abline(v=mean(B), lwd = 2, lty=3, col="darkblue")
abline(v=median(B), lwd = 2, lty=3, col="red")
abline(v=mfv(B), lwd = 2, lty=3, col="yellow")
abline(v=var(B), lwd = 2, lty=3, col="green")
abline(v=sd(B), lwd = 2, lty=3, col="orange")
abline(v=(sd(B)/mean(B)), lwd = 2, lty=3, col="gray")
abline(v=skew(B), lwd = 2, lty=3, col="pink")
abline(v=kurtosi(B), lwd = 2, lty=3, col="purple")

# inciso e) Diagrama de caja y bigotes

boxplot(A,
        main = "Resistencia de barras A ",
        col = "pink",
        border = "purple")
#No hay observaciones atipicas

boxplot(B,
        main = "Resistencia de barras B",
        col = "pink",
        border = "purple")
#No hay observaciones atipicas

# inciso f) Conclusiones
Barras <- table(A, B)
Resistencia <- prop.table(Barras, margin = 1)
barplot(Resistencia,
        main = "Resistencia de los tipos de barra A y B",
        xlab = "Resistencia",
        ylab = "Proporciones",
        col = c("royalblue", "grey"))
legend(x = "topright", legend = c("Tipo A", "Tipo B"), fill = c("royalblue", "grey"),
       title = "Proporciones de resistencias")


#En conclusion,los datos son homogeneos,tienen sesgo negativo y de distribucion platicurtica,
#tambien pudimos ver que las proporciones de resistencia de tipo A estan dentro del
#0.0 y 2.0 %, y las proporcines de resistencia de tipo B estan dentro de 0.0 y 1.0 %
#Haciendo referencia al numero de observaciones con una caracteristica en particular entre
#la poblacion de referencia
######################################################################













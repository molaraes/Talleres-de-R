# =========================================================
# Pruebas de hipótesis
# Fecha: 17/02/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

#Paquetes
pacman::p_load("haven", "readxl", "tidyverse")

#Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Lectura de datos
base_marginacion <- read_excel("IMM_2020.xlsx", 
                               sheet = "IMM_2020")


# Prueba de hipótesis para una media --------------------------------------

mean(base_marginacion$IM_2020)

#El promedio del índice de marginación a nivel municipal es de 53.95558(miuo). Nosotros queremos demostrar, a partir de nuestra muestra, que el promedio del índice de marginación no es igual a ese valor. Para corroborarlo debemos hacer una prueba de hipótesis con un nivel de confianza del 95%.

#Ho:xbarra es igual a miuo
#Ha:xbarra es diferente a miuo

#Vamos a hacer una muestra
set.seed(1234)

muestra <- base_marginacion[sample(nrow(base_marginacion), size=1235), ]

#Datos del problema
n <- 1235   # Tamaño de la muestra
xbarra <- mean(muestra$IM_2020) # Media muestral
sd <- 3.90459 # Desviación estándar
miuo <- 53.95558 # Valor hipotético de la media poblacional (Ho)
alfa <- 0.05  # Nivel de significancia (1 - nivel de confianza)
se <- sd/sqrt(n) # Error estándar

#Dos reglas decisión
#T crítico
test <- (xbarra-miuo)/se #Estadístico t
t<- qt(alfa/2, df=n-1, lower.tail=T) #t de tablas
# qt() da el valor t 
# alfa/2 porque es prueba bilateral
# df=n-1 son los grados de libertad
# lower.tail=T indica que queremos el valor de la cola inferior

#Nos preguntamos si ¿test es mayor que t?
test > t
#No rechazo

#Pvalor
pvalor <- 2*pt(abs(test), df=n-1, lower.tail=F)
# pt() da la probabilidad acumulada
# abs(test) usamos el valor absoluto del estadístico
# multiplicamos por 2 porque es prueba bilateral
# lower.tail=F indica que queremos la probabilidad de la cola superior (ya que es el valor absoluto)

#¿El p valor es menor o mayor a 0.05?

#Hacemos la prueba
t.test(mu=53.95558, muestra$IM_2020, conf.level= 0.95)


# Prueba de hipótesis proporción ------------------------------------------

#La proporción de municipios rurales en México es de 0.1498582. Nosotros queremos demostrar, a partir de una muestra, que la proporción no es igual a ese valor.

#Para corroborarlo, debemos hacer una prueba de hipótesis con un nivel de confianza del 95%

#Datos
n <- 1235 # Tamaño de muestra
x <- 195  # Número de municipios rurales
p_muestra <- x/n  # Proporción muestral
p0 <- 0.1498582 # Proporción hipotética (Ho)
alfa <- 0.05  # Nivel de significancia
se <- sqrt((p_muestra*(p_muestra))/n) # Error estándar para proporciones

#Reglas de decisión
#Valor Z
z_emp <- (p_muestra - p0)/se    # Z empírico
z_tabla <- qnorm(alfa/2)   # Valor crítico (dividimos alfa/2 por ser bilateral)

#¿Z empírico es mayor o menor que la Z de tabla?

#P valor
p_valor <- 2*pnorm(abs(z_emp), lower.tail=FALSE)  # Multiplicamos por 2 por ser bilateral

#¿El p valor es mayor o menor a 0.05?

#Ahora lo hacemos con prop.test
table(muestra$rural)
prop.test(p= 0.1498582, x = 195, n = 1235, alternative = c("two.sided"), correct= FALSE)



# =========================================================
# Intervalos de confianza para medias y proporciones
# Fecha: 13/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

#Instalamos nuestros paquetes
install.packages("pacman")
pacman::p_load("haven", "tidyverse")

#Establecemos nuestro directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Abrimos nuestros datos
encig <- read_csv("encig2023_01_sec1_A_3_4_5_8_9_10.csv")


# Intervalo de confianza para la media con varianza desconocida -----------

#Paso 1. Calculamos el estimador puntual (media)

#La variable se refiere a una escala del 0 al 10 en qué tanta confianza tienen las personas en el Gobierno Federal.

#En primer lugar, la convertimos a numérica.
class(encig$A1_5)
encig$A1_5 <- as.numeric(encig$A1_5)

#La vemos
summary(encig$A1_5)

#Vamos a filtrar la base para todos aquellos casos válidos.
base_modif <- encig %>% 
  filter(A1_5 < 99)

#Calculamos nuestro estimador: la media
xbarra <- mean(base_modif$A1_5)
xbarra

#Paso 2: Calcular nuestro error estándar
n <- 38673 #tamaño de muestra
sd <- sd(base_modif$A1_5) #desviación estándar
se <- sd/sqrt(n) #error estándar

#Paso 3: Valor de t y grados de libertad
gl <- n-1
gl

#Nos vamos a la tabla y vemos que nuestro "t teórico" correspondiente a nuestro valor de "t calculado" con n-1 grados de libertad, es de 1.96 para un nivel de 95% de confianza.

t90 <- 1.645
t95 <- 1.96
t99 <- 2.576


#Paso 4: calculamos el margen de error
margen90 <- t90*se
margen95 <- t95*se
margen99 <- t99*se

#Paso 5: calculamos nuestro intervalo de confianza

#Al 95%
inferior95 <- xbarra-margen95
superior95 <- xbarra+margen95

#Al 90%
inferior90 <- xbarra-margen90
superior90 <- xbarra+margen90

#Al 99%
inferior99 <- xbarra-margen99
superior99 <- xbarra+margen99

#En este caso, con una muestra de 38673 tenemos que, el parámetro poblacional se encuentra entre los valores de 5.92 y 5.98, a un nivel de confianza del 95%. Si obtuviéramos muestras sucesivas, se esperaría que en un 95% de ellas, se encuentre nuestro parámetro poblacional sobre la confianza en el Gobierno Federal.

#Directo
l.model <- lm(A1_5 ~ 1, base_modif)
confint(l.model, level=0.95)

#El lm() ajusta un modelo lineal. La fórmula tiene la forma respuesta ~ predictores.

#A1_5 ~ 1: El modelo está ajustando solo una constante, es decir, un modelo en el que no hay predictores (variables explicativas) y solo se está estimando el valor medio de la variable dependiente A1_5 Esto es equivalente a calcular la media de A1_5, pero dentro de un marco de regresión lineal.

#~ 1 indica que solo se incluirá el término constante (el intercepto) y no se incluirán otras variables como predictores.

#Otra forma rápida
t.test(encig2$A1_5, mu=0, alternative=c("two.sided"))

# Intervalo de confianza para una proporción ------------------------------

#Paso 1. Calcular estimador puntual (la proporción)

#La variable es la frecuencia de corrupción. La pregunta es:

#La corrupción es una práctica que sucede cuando un servidor público o empleado del gobierno abusa de sus funciones para obtener beneficios personales como dinero, regalos o favores por parte del ciudadano. Por lo que usted sabe, en (ESTADO) estas prácticas son: muy frecuentes, frecuentes, poco frecuentes, nunca se dan.

#Convertiremos esa variable en una variable dummy.
base_modif <- base %>% 
  filter(P3_2!=9) %>% #omitiendo casos que no contestan
  mutate(corrup = ifelse(P3_2 <= 3,1,0))

#La vemos
tabla <- table(base_modif$corrup)

#Calculamos la proporción de las personas que consideran la corrupción como frecuente.

#A mano
n<-641+37509  
pgorro <- 37509/n
pgorro

#Automático
proporcion_1 <- tabla["1"] / sum(tabla)
proporcion_1

# Paso 2. Calcular error estándar
n<- 38150
sd <- sqrt(pgorro*(1-pgorro))
se <- sd/sqrt(n)

#Paso 3. Establecemos z
z90 <- 1.64
z95 <- 1.96
z99 <- 2.58

#Paso 4. Margen de error
margen90 <- se*z90
margen95 <- se*z95
margen99 <- se*z99

#Paso 5. Construimos el intervalo al 95%
inferior90 <- pgorro-margen90
superior90 <- pgorro+margen90

inferior95 <- pgorro-margen95
superior95 <- pgorro+margen95

inferior99 <- pgorro-margen99
superior99 <- pgorro+margen99

#Manera rápida
prop.test(37509, 38150, p=NULL, 
          alternative=c("two.sided"), 
          conf.level=0.95)


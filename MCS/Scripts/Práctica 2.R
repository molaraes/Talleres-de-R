# =========================================================
# Práctica: intervalo de confianza para la proporción y pruebas de hipótesis
# Fecha: 06/03/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

install.packages("pacman")
pacman::p_load("tidyverse", "haven", "patchwork")

# Quitamos notación científica
options(scipen=999)

# Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/MCS/Datos/")

# Lectura de datos
mex_2023<- read_dta("mex_2023.dta")

# Intervalo de confianza para la proporción (datos reales) ------------------

#Escoge dos variables de la base de datos, para cada una:

#1. Elabora la tabla de frecuencias
#2. Calcula el intervalo de confianza para el 90%, 95% y 99%.
#3. ¿Qué significan los límites inferiores y superiores para cada nivel de confianza y de cada variable?
#4. Grafica los intervalos (90, 95, 99) para las dos variables que escogiste. Grafica los tres intervalos de una variable en un gráfico y los tres intervalos de la otra variable en otro gráfico. Junta ambos utilizando el paquete patchwork.
#5. ¿Qué sucede con el intervalo cuando aumentas el nivel de confianza?
#6. Escribe la hipótesis nula y la hipótesis alternativa para cada uno de los siguiente:

#Un investigador afirma que la edad promedio de los migrantes internacionales que llegan a la Ciudad de México es mayor a 35 años. Con una muestra de 200 migrantes recientes, determina las hipótesis para verificar esta afirmación.

#Un estudio previo mostró que el 45% de los hogares en zonas rurales tiene acceso a internet. Un nuevo programa social afirma haber modificado este porcentaje. Con una muestra de 500 hogares rurales, establece las hipótesis para determinar si el porcentaje ha cambiado.

#La tasa de desempleo juvenil en una región se ha mantenido históricamente en 12%. Después de implementar un programa de primer empleo, se quiere probar si la tasa ha disminuido. Con datos de 300 jóvenes, establece las hipótesis.

#Las autoridades afirman que el tiempo promedio de espera para trámites en oficinas gubernamentales es igual a 45 minutos. Un grupo ciudadano sospecha que es diferente. Con una muestra de 150 usuarios, plantea las hipótesis.

#Un programa de transferencias monetarias condicionadas afirma que ha logrado reducir la deserción escolar a menos del 8% en secundarias públicas. Con datos de 400 estudiantes, establece las hipótesis para verificar esta afirmación.

#Históricamente, el 60% de las mujeres en edad reproductiva en cierta región tienen acceso a servicios de salud reproductiva. Después de una intervención gubernamental, se quiere probar si este porcentaje ha aumentado. Con una muestra de 600 mujeres, establece las hipótesis.

#Los datos oficiales indican que el ingreso promedio mensual de trabajadores informales es de $4,500 pesos. Un investigador sospecha que este dato no es preciso y podría ser diferente. Con una muestra de 250 trabajadores informales, plantea las hipótesis.


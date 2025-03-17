# =========================================================
# Pruebas de hipótesis para la diferencia de proporciones
# Fecha: 20/03/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Pruebas de hipótesis datos simulados ------------------------------------

options(scipen=999)

# Haremos una simulación para comparar actitudes hacia políticas redistributivas entre progresistas y conservadores.

# Vamos a definir las proporciones de apoyo a políticas redistributivas en cada grupo.
p.progresistas <- 0.85  # 85% de los progresistas apoyan políticas redistributivas
N.progresistas <- 35000000  # Población de progresistas

p.conservadores <- 0.30  # 30% de los conservadores apoyan políticas redistributivas
N.conservadores <- 45000000  # Población de conservadores

# Generamos las poblaciones según su postura.
progresistas.que.apoyan <- rep(1, N.progresistas * p.progresistas)  # Apoyan redistribución
progresistas.que.no.apoyan <- rep(0, N.progresistas * (1 - p.progresistas))  # No apoyan

conservadores.que.apoyan <- rep(1, N.conservadores * p.conservadores) #Apoyan
conservadores.que.no.apoyan <- rep(0, N.conservadores * (1 - p.conservadores)) #No apoyan

# Combinamos los grupos
progresistas <- c(progresistas.que.apoyan, progresistas.que.no.apoyan)
table(progresistas)

conservadores <- c(conservadores.que.apoyan, conservadores.que.no.apoyan)
table(conservadores)

# Ahora hacemos la muestra: primero definimos tamaño de muestra y nivel de significancia

n <- 1000  # Tamaño de la muestra
alpha <- 0.05  # Nivel de significancia

# Extraemos muestras aleatorias
set.seed(123)
muestra.1 <- sample(progresistas, size = n)
set.seed(456)
muestra.2 <- sample(conservadores, size = n)

# Estimamos las proporciones muestrales
p.hat.1 <- mean(muestra.1)
p.hat.1

p.hat.2 <- mean(muestra.2)
p.hat.2

# Calculamos el estadístico de prueba (z) para diferencia de proporciones
x.1 <- sum(muestra.1)  # Casos favorables en muestra 1
x.2 <- sum(muestra.2)  # Casos favorables en muestra 2
p.hat <- (x.1 + x.2) / (n+n)  # Proporción combinada

z <- (p.hat.1-p.hat.2)/sqrt(p.hat*(1-p.hat)*((1/n)+(1/n)))

# Calculamos el valor p

# Recordemos que el valor p, en este caso, representa la probabilidad de obtener una diferencia de proporciones entre progresistas y conservadores igual o más extrema que la observada en la muestra, suponiendo que en realidad no hay diferencia entre ambos grupos en la población (es decir, suponiendo que la hipótesis nula es verdadera).

p.val <- 1-pnorm(z)
p.val <- pnorm(z, lower.tail = FALSE)  

# Tomamos la decisión estadística
decision <- ifelse(p.val <= alpha, "Rechazar H0", "No rechazar H0")

# Mostramos los resultados
decision  # Decisión de la prueba

#Corolario: explicación de p valor

# Cargar librerías
library(tidyverse)

# Definir la distribución normal estándar
x <- seq(-4, 4, length=1000)
y <- dnorm(x, mean=0, sd=1) #nos da la densidad de la curva

# Definir el valor de la estadística de prueba z
z_observado <- 2

# Crear el dataframe para ggplot
datos <- data.frame(x = x, y = y)

# Crear el gráfico
ggplot(datos, aes(x, y)) +
  geom_line(color = "blue", size = 1) +  # Curva normal
  geom_area(data = subset(datos, x >= z_observado), aes(y = y), 
            fill = "red", alpha = 0.5) +  # Sombreado del valor p
  geom_vline(xintercept = z_observado, linetype = "dashed", color = "black") + # Línea en z
  labs(title = "Distribución Normal y Valor p",
       x = "Z", y = "Densidad de Probabilidad") +
  theme_minimal()


# Simulación: repetimos el experimento 1000 veces para evaluar la estabilidad del resultado
n <- 1000
alpha <- 0.05
decisiones <- c() #creamos vector vacío
set.seed(891)

for(i in 1:1000) {
  muestra.1 <- sample(progresistas, size = n)
  muestra.2 <- sample(conservadores, size = n)
  
  p.hat.1 <- mean(muestra.1)
  p.hat.2 <- mean(muestra.2)
  
  x.1 <- sum(muestra.1)
  x.2 <- sum(muestra.2)
  p.hat <- (x.1 + x.2) / (n + n)
  
  z <- (p.hat.1-p.hat.2)/sqrt(p.hat*(1-p.hat)*((1/n)+(1/n)))
  
  p.val <- pnorm(z, lower.tail = FALSE)
  
  decisiones[i] <- ifelse(p.val <= alpha, "Rechazar H0", "No rechazar H0")
  
}

# Tabla con resultados de las 1000 simulaciones
table(decisiones)

# Pruebas de hipótesis datos reales ------------------------------------

#Abrimos la base de datos de LAPOP México
library(haven)

choose.dir()

setwd("C:\\Users\\molar\\Dropbox\\2025_Trabajos\\FLACSO\\MCS\\Datos")

mex_2023 <- read_dta("mex_2023.dta")

#Veamos la diferencia entre la proporción de personas que viven en zonas rurales y urbanas que consideran que la gente es muy confiable 

table(mex_2023$it1, mex_2023$ur)

x.1 <- 265   #personas en localidades urbanas que piensan que la gente es muy confianble
n.1 <- 265+438+430+152 #total de personas en localidades urbanas

x.2 <- 66   #personas en localidades rurales que piensan que la gente es muy confianble
n.2 <- 66+123+116+19 #total de personas en localidades rurales

#prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE)

#x: Representa el número de éxitos o casos positivos observados. Puede ser un vector si estás comparando múltiples proporciones.
#n: Es el número total de ensayos o tamaño de la muestra. También puede ser un vector correspondiente a cada valor en x.
#p = NULL: Es la proporción hipotética bajo la hipótesis nula. Si no se especifica (NULL), la función asume que las proporciones son iguales en todos los grupos.
#alternative = c("two.sided", "less", "greater"): Especifica el tipo de hipótesis alternativa
#conf.level = 0.95: Define el nivel de confianza para el intervalo calculado (por defecto es 0.95, lo que corresponde a un 95% de confianza).
#correct = TRUE: Indica si se debe aplicar la corrección de continuidad de Yates, que es una ajuste para mejorar la aproximación de la distribución discreta binomial a la distribución continua chi-cuadrado.

#Cola derecha
prop.test(c(x.1, x.2), c(n.1, n.2), 
          alternative = c("greater"), 
          conf.level = 0.95, correct = FALSE)

#Cola izquierda
prop.test(c(x.1, x.2), c(n.1, n.2), 
          alternative = c("less"), 
          conf.level = 0.95, correct = FALSE)

#Dos colas
prop.test(c(x.1, x.2), c(n.1, n.2), 
          alternative = c("two.sided"), 
          conf.level = 0.95, correct = FALSE)

# Graficamos

# Datos del intervalo de confianza del 95%
limite_inferior <- -0.04659591
limite_superior <- 0.05163986

# Crear un data frame para el gráfico
datos <- data.frame(
  x = c(limite_inferior, limite_superior),
  y = c(1, 1)
)

# Crear el gráfico
ggplot() +
  # Añadir la línea horizontal para el intervalo de confianza
  geom_line(data = datos, aes(x = x, y = y), size = 1.5) +
  # Añadir puntos en los extremos del intervalo
  geom_point(data = datos, aes(x = x, y = y), size = 3) +
  # Añadir una línea vertical roja en x = 0
  geom_vline(xintercept = 0, color = "red", size = 1, linetype = "dashed") +
  # Etiquetas y títulos
  labs(
    title = "Intervalo de confianza del 95% para la diferencia de proporciones",
    subtitle = "Prueba de igualdad de proporciones (p-value = 0.9201)",
    x = "Diferencia de proporciones (prop1 - prop2)",
    y = ""
  ) +
  # Ajustar los límites del eje x para que se vea bien el intervalo
  scale_x_continuous(
    limits = c(min(limite_inferior * 1.2, -0.06), max(limite_superior * 1.2, 0.06)),
    breaks = seq(-0.06, 0.06, 0.02)
  ) +
  # Eliminar la escala del eje y
  scale_y_continuous(breaks = NULL) +
  # Añadir anotaciones para los límites
  annotate("text", x = limite_inferior, y = 0.95, 
           label = sprintf("%.4f", limite_inferior), vjust = 1) +
  annotate("text", x = limite_superior, y = 0.95, 
           label = sprintf("%.4f", limite_superior), vjust = 1) +
  # Añadir anotación para el cero
  annotate("text", x = 0, y = 0.95, 
           label = "0", color = "red", vjust = 1) +
  # Usar un tema minimalista
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Ejercicio ---------------------------------------------------------------

#1. Repetir el ejercicio de datos simulados con 1000 muestras de tamaño 500
#2. Repetir el ejercicio de datos reales con otras variables de su interés.

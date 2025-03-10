# =========================================================
# Pruebas de hipótesis
# Fecha: 13/03/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Pruebas de hipótesis datos simulados ------------------------------------

#Vamos a simular una población y vamos a considerar la proporción de personas que se encuentra a favor de la portación de armas y en contra.

#p será nuestro verdadero parámetro
p <- 0.50

#Tamaño de la población
N <- 232000000

#Ponemos un 1 si aprueban la portación de armas y 0 en caso contrario.
aprueban <- rep(1, p*N)
reprueban <- rep(0, (1-p)*N)
poblacion <- c(aprueban, reprueban)

#Definimos un nivel de significancia
alpha <- 0.05

#Definimos el valor nulo de p(contraste)
p.0 <- 0.5

#Tomamos una muestra de la población de tamaño 1000.
n <- 1000
set.seed(123)
muestra <- sample(poblacion, size=n)

#Calculamos nuestro estimador
p.hat <- sum(muestra)/length(muestra)

#Calculamos nuestro estadístico de prueba 
z <- (p.hat-p.0)/sqrt(p.0*(1-p.0)/n)

#Escenario 1: Queremos comprobar a partir de nuestra muestra si más de la mitad de las personas aprueban la portación de armas.

#¿Qué tipo de cola es?
#Planteamiento:
#Ho: p<=0.5
#Ha: p>0.5

p.value <- pnorm(z, lower.tail=F) #opción 1
p.value
p.value <- 1-pnorm(z) #opción 2
p.value

#¿Rechazamos o no rechazamos? ¿A qué conclusión llegamos con nuestros datos?

#Escenario 2: queremos comprobar, a partir de nuestra muestra, si menos de la mitad de las personas aprueban la portación de armas.

#¿Qué tipo de cola es?
#Planteamiento:
#Ho: p >= 0.5
#Ha: p < 0.5

p.value <- pnorm(z, lower.tail=T)
p.value
p.value <- pnorm(z)
p.value

#¿Rechazamos o no rechazamos? ¿A qué conclusión llegamos con nuestros datos?

#Escenario 3: queremos comprobar, a partir de nuestra muestra, que la proporción de personas que aprueban la portación de armas es diferente al 50%

#¿Qué tipo de cola es?
#Planteamiento:
#Ho: p = 0.5
#Ha: p != 0.5 (diferente)
p.value <- 2*pnorm(abs(z), lower.tail=F)
p.value

#Tomamos una decisión, haciendo la prueba de dos colas, comparando el valor p con el nivel de significancia
decision <- ifelse(p.value<=alpha, "rechazar H0", "no rechazar H0")
decision

#Repetimos el experimento tomando mil muestras de tamaño 1000. A un nivel de significancia del 5%

#Primero los vectores
n <- 1000
alpha <- 0.05
decisiones <- c()
set.seed(123)


for(i in 1:1000){ #Para cada i desde 1 hasta 1000
  muestra <- sample(poblacion, size=n) #Calcula una muestra de tamaño n
  p.hat <- sum(muestra)/n #Calcula estimador
  z <- (p.hat-p.0)/sqrt(p.0*(1-p.0)/n) #Calcula estadístico de prueba
  p.val <- 2*pnorm(abs(z), lower.tail=F) #Calcula p valor
  decision <- ifelse(p.val<=alpha, "rechazar H0",
                     "no rechazar H0") #Toma una decisión
  decisiones[i] <- decision #Rellena el vector
}

table(decisiones)

# Pruebas de hipótesis datos reales ------------------------------------

#Abrimos la base de datos de LAPOP México
library(haven)
library(tidyverse)

choose.dir()
setwd("C:\\Users\\molar\\Dropbox\\2025_Trabajos\\FLACSO\\MCS\\Datos")
mex_2023 <- read_dta("mex_2023.dta")

#Ahora, hablando de la gente de por aquí, ¿diría que la gente de su comunidad es muy confiable, algo confiable, poco confiable o nada confiable?

#Veamos la proporción de personas que consideran que la gente es muy confiable y en un ejercicio hipotético, imaginemos que ese será nuestra p.0
table(mex_2023$it1)
p.0 <- 331/(331+561+546+171)

#Ahora saquemos una muestra de 500
set.seed(123)
muestra <- mex_2023[sample(nrow(mex_2023), size=500), ]

#nrow(mex_2023): Obtiene el número total de filas del dataframe mex_2023
#sample(nrow(mex_2023), size=500): Selecciona aleatoriamente 500 números dentro del rango de filas
#mex_2023[... , ]: Accede a las filas seleccionadas en el dataframe
#muestra <-: Asigna el resultado a un nuevo dataframe llamado muestra

#Ahora hacemos la tabla con nuestra muestra
table(muestra$it1)
x <- 110
n <- 110+173+159+54

#prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE)

#x: Representa el número de éxitos o casos positivos observados. Puede ser un vector si estás comparando múltiples proporciones.
#n: Es el número total de ensayos o tamaño de la muestra. También puede ser un vector correspondiente a cada valor en x.
#p = NULL: Es la proporción hipotética bajo la hipótesis nula. Si no se especifica (NULL), la función asume que las proporciones son iguales en todos los grupos.
#alternative = c("two.sided", "less", "greater"): Especifica el tipo de hipótesis alternativa
#conf.level = 0.95: Define el nivel de confianza para el intervalo calculado (por defecto es 0.95, lo que corresponde a un 95% de confianza).
#correct = TRUE: Indica si se debe aplicar la corrección de continuidad de Yates, que es una ajuste para mejorar la aproximación de la distribución discreta binomial a la distribución continua chi-cuadrado.

#Cola derecha
prop.test(x, n, p = p0, 
          alternative = c("greater"), 
          conf.level = 0.95, correct = FALSE)

#Cola izquierda
prop.test(x, n, p = p0, 
          alternative = c("less"), 
          conf.level = 0.95, correct = FALSE)

#Dos colas
prop.test(x, n, p = p0, 
          alternative = c("two.sided"), 
          conf.level = 0.95, correct = FALSE)

# Graficamos

# Datos del intervalo de confianza para cola derecha
etiquetader <- ("Cola derecha")
p_estimadoder <- 0.2217742      # Proporción estimada
ic_inferiorder <- 0.192647      # Límite inferior del IC al 95%
ic_superiorder <- 1.000000     # Límite superior del IC al 95%

# Datos del intervalo de confianza para cola izquierda
etiquetaizq <- ("Cola izquierda")
p_estimadoizq <- 0.2217742       # Proporción estimada
ic_inferiorizq <- 0.0000000      # Límite inferior del IC al 95%
ic_superiorizq <- 0.2539203     # Límite superior del IC al 95%

# Datos del intervalo de confianza para dos colas
etiquetados <- ("Dos colas")
p_estimadodos <- 0.2217742        # Proporción estimada
ic_inferiordos <- 0.1874297      # Límite inferior del IC al 95%
ic_superiordos <- 0.2603952     # Límite superior del IC al 95%

# Crear un dataframe para el gráfico
datos <- data.frame(
  etiqueta = c(etiquetader, etiquetaizq, etiquetados),
  estimacion = c(p_estimadoder, p_estimadoizq, p_estimadodos),
  inferior = c(ic_inferiorder,ic_inferiorizq,ic_inferiordos),
  superior = c(ic_superiorder, ic_superiorizq,ic_superiordos)
)

# Gráfico con orientación horizontal
ggplot(datos, aes(y = etiqueta, x = estimacion)) +
  # Punto para la estimación puntual
  geom_point(size = 3, color = "purple") +
  # Barra de error horizontal para el intervalo de confianza
  geom_errorbarh(aes(xmin = inferior, xmax = superior), height = 0.2, linewidth = 0.6, color = "black") +
geom_vline(xintercept = 0.2057178, linetype = "dashed", color = "red")+
  # Personalización del gráfico
  labs(
    title = "Pruebas de hipótesis para para la Proporción",
    y = "",
    x = "Proporción"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

# Ejercicio ---------------------------------------------------------------

#1. Repetir el ejercicio de datos simulados con 10000 muestras de tamaño 1000
#2. Repetir el ejercicio de datos reales con otra variable de su interés.

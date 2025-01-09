# =========================================================
# Parámetros y estimadores
# Fecha: 06/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------
library(tidyverse)


# Parámetros y estimadores ------------------------------------------------

# 1. Simulamos una población con 1000 observaciones
set.seed(42)  # Para reproducibilidad
poblacion <- rnorm(1000, mean = 50, sd = 10)  # Población con media 50 y desviación estándar 10

# 2. Parámetros de la población (conocidos)
parametro_media <- mean(poblacion)
parametro_desviacion <- sd(poblacion)
parametro_varianza <- var(poblacion)


cat("Media de la población (mu):", parametro_media, "\n")
cat("Desviación estándar de la población (sigma):", parametro_desviacion, "\n")
cat("Varianza de la población (sigma cuadrada):", parametro_varianza, "\n")

# 3. Ahora tomamos una muestra aleatoria de tamaño 100 de esa población
muestra <- sample(poblacion, 100)

# 4. Estimadores (media y desviación estándar muestral)
estimador_media <- mean(muestra)  # Media muestral
estimador_desviacion <- sd(muestra)  # Desviación estándar muestral

cat("Media muestral:", estimador_media, "\n")
cat("Desviación estándar muestral:", estimador_desviacion, "\n")

# Características de los estimadores --------------------------------------

# 1. Insesgados

# 1. Simulamos la misma población con 1000 observaciones
set.seed(42)  # Para reproducibilidad
poblacion <- rnorm(1000, mean = 50, sd = 10)  
# Población con media 50 y desviación estándar 10

# 2. Calculamos la media de la población
media_poblacion <- mean(poblacion)

# 3. Tomamos 100 muestras de tamaño 30 y calculamos la media de cada muestra
n_muestras <- 100
tamaño_muestra <- 30
medias_muestrales <- numeric(n_muestras)
for (i in 1:n_muestras) {
  muestra <- sample(poblacion, tamaño_muestra) #elabora la muestra
  medias_muestrales[i] <- mean(muestra) #calcula la media para cada muestra
}

# 4. Creamos un dataframe para graficar
df <- data.frame(
  media_muestral = medias_muestrales
)

# 5. Graficamos las distribuciones 
ggplot(df, aes(x = media_muestral)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = media_poblacion), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución de la Media Muestral vs Media de la Población",
    x = "Media muestral",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12)
  ) +
  annotate("text", x = media_poblacion + 1, y = 10, label = paste("Media población =", round(media_poblacion, 2)), color = "red")

# 2. Eficientes:

# 1. Simulamos una población con 1000 observaciones
set.seed(42)  # Para reproducibilidad
poblacion <- rnorm(1000, mean = 50, sd = 10)  
# Población con media 50 y desviación estándar 10

# 2. Calculamos la varianza de la población (sigma^2)
varianza_poblacion <- var(poblacion)

# 3. Tomamos 100 muestras de tamaño 30 y calculamos la varianza
n_muestras <- 100
tamaño_muestra <- 30
varianzas_muestrales <- numeric(n_muestras)  

#La función numeric() se usa para crear un vector de números. El argumento que recibe es la longitud del vector. Estamos pidiendo que cree un vector de tamaño n_muestras. Este vector estará inicializado con ceros.

for (i in 1:n_muestras) {
  muestra <- sample(poblacion, tamaño_muestra)  # Tomamos una muestra aleatoria de la población
  varianzas_muestrales[i] <- var(muestra)  # Calculamos la varianza de la muestra
}

# 4. Calculamos la varianza promedio de las varianzas muestrales
varianza_promedio_muestral <- mean(varianzas_muestrales)

# 5. Comparamos la varianza de la población con la varianza promedio de las varianzas muestrales
cat("Varianza de la población (sigma^2):", varianza_poblacion, "\n")
cat("Varianza promedio de las varianzas muestrales:", varianza_promedio_muestral, "\n")

# 6. Graficamos la distribución de las varianzas muestrales

df <- data.frame(
  varianza_muestral = varianzas_muestrales
)

ggplot(df, aes(x = varianza_muestral)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = varianza_poblacion), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución de las Varianzas Muestrales",
    x = "Varianza muestral",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12)
  ) +
  annotate("text", x = varianza_poblacion + 5, y = 10, label = paste("Varianza población =", round(varianza_poblacion, 2)), color = "red")

#Ojo, repaso: las diferencias entre estimadores y parámetros dan cuenta de errores en estimación. Las estimaciones más comunes son la media, proporción y varianza.

# Práctica ----------------------------------------------------------------
#Imagina que eres parte de un equipo de investigación en una ONG que está estudiando la desigualdad económica en una ciudad. La organización necesita un análisis preciso sobre la distribución del ingreso para proponer políticas públicas que ayuden a reducir las brechas entre las diferentes clases socioeconómicas.

#Para ello, se te ha encomendado la tarea de analizar los datos de una población de 10,000 habitantes y evaluar la distribución del ingreso. Sin embargo, debido a limitaciones de recursos, solo puedes obtener una muestra representativa de 1,000 individuos.

#Las fases del proyecto que deben desarrollar son:
#1. Creación de la población de estudio.
#2. Cálculo de los parámetros poblacionales.
#3. Estimación de la muestra y estimadores puntuales.
#4. Comparación de parámetros poblacionales y estimadores.
#5. Conclusión.

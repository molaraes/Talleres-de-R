# =========================================================
# Introducción a la inferencia
# Fecha: 20/02/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------
pacman::p_load("tidyverse", "patchwork")


# Parámetros y estimadores ------------------------------------------------

#Recordemos que los parámetros se refieren a la población y los estimadores a la muestra. Normalmente no conocemos los parámetros y por eso hacemos inferencia estadística. Los estimadores cumplen con dos características: son insesgados y minimizan la varianza.

#Vamos a crear una población de 94 millones de personas donde el 60% aprueba la gestión del Presidente.

N <- 94000000 #Población
p <- 0.6      #60% que aprueba (parámetro)

#Vamos a poner 1 si las personas aprueban y 0 si desaprueban
aprueban <- rep(1, p*N) # Crea vector de 1's representando a quienes aprueban
desaprueban <- rep(0,(1-p)*N) # Crea vector de 0's representando a quienes desaprueban
poblacion <- c(aprueban, desaprueban) #simulamos nuestra población

#Exploramos nuestro "objeto" población
length(poblacion) #cuántas observaciones tiene
table(poblacion) #frecuencia absoluta de las personas que aprueban y desaprueban la gestión

#Vamos a extraer una muestra de tamaño 10
n <- 10
muestra <- sample(poblacion, size=n)
muestra

#Utilizamos nuestra muestra para construir un estimador puntual de p (proporción de la muestra)
p_gorro <- sum(muestra)/length(muestra) #suma los 1 y los divide entre el tamaño de la muestra
p_gorro

#Ahora vamos a extraer 1000 muestras de tamaño 10 y estimaremos 1000 veces nuestro pgorro
n <- 10                     # Define el tamaño de muestra como 10
estimaciones.10 <- c()      # Crea un vector vacío para almacenar las estimaciones
set.seed(123)               # Fija una semilla para hacer reproducibles los resultados aleatorios

for(i in 1:1000) {          # Bucle que se repite 1000 veces
  muestra<- sample(poblacion, size=n)     # Toma una muestra aleatoria de tamaño n de la población
  muestra                                 # Enseña la muestra actual (aunque no se guarda)
  p_gorro <- sum(muestra)/length(muestra) # Calcula la proporción muestral 
  estimaciones.10[i] <- p_gorro           # Guarda la estimación en la posición i del vector
}

estimaciones.10            # Muestra todas las estimaciones
min(estimaciones.10)       # Encuentra el valor mínimo de todas las estimaciones
max(estimaciones.10)       # Encuentra el valor máximo de todas las estimaciones

#Lo guardo como un data frame para graficar
estimaciones.10 <- as.data.frame(estimaciones.10)

#Gráfico
estimaciones.10 %>% 
  ggplot(aes(x=estimaciones.10))+
  geom_density()+
  theme_minimal()

#¿Qué pasa si hacemos muestras de tamaño 30, tamaño 100?
#¿Cuál es el promedio de cada una de las estimaciones y cuál se asemeja más al parámetro poblacional?


# Teorema del Límite Central ----------------------------------------------

#El Teorema Central del Límite (TCL) establece que cuando el tamaño de muestra es suficientemente grande (generalmente n ≥ 30), la distribución muestral sigue aproximadamente una distribución normal, independientemente de la distribución de la población original.

#Ejemplo de dados.
#Sabemos que un dado tiene un promedio de 3.5
#Supongamos que lanzamos el dado 10000 veces

resultado <- sample(1:6, 10000, replace=T)

#Vamos a sacar muestras de tamaño 10 de las 10000 observaciones anteriores.

#Haremos este ejercicio k veces, k=10000

x10 <- c() #declaramos un vector
k<- 10000
set.seed(123)

#Para cada valor i desde 1 hasta k, obten una muestra de tamaño 10 y calcula la media. 

for ( i in 1:k) {
  x10[i] = mean(sample(1:6,10, replace = TRUE))}


# Crear un data.frame para ggplot
df <- data.frame(x10 = x10)

# Crear el histograma con ggplot2
ggplot(df, aes(x = x10)) +
  geom_histogram(binwidth = 0.2, fill = "pink", color = "black", alpha = 0.7) +  # Histograma
  geom_vline(aes(xintercept = mean(x10)), color = "red", linetype = "solid", size = 1) +  # Línea en la media muestral
  geom_vline(xintercept = 3.5, color = "blue", linetype = "dashed", size = 1) +  # Línea en el valor esperado
  labs(title = "Muestra = 10", x = "Resultado", y = "Frecuencia") +
  theme_minimal()  # Tema minimalista

#Si n tiende a infitnito, podemos obtener una distribución normal por el TLC
x30 <- c()
x100 <- c()
x1000 <- c()
k =10000
set.seed(123)

#Para cada i desde 1 hasta k genera una muestra de tamaño 30, 100 y 1000 con números del 1 al 6 con reemplazo. Calcula la media para cada uno.

for ( i in 1:k){ 
  x30[i] = mean(sample(1:6,30, replace = TRUE))
  x100[i] = mean(sample(1:6,100, replace = TRUE))
  x1000[i] = mean(sample(1:6,1000, replace = TRUE))
}

# Crear los tres histogramas
p1 <- ggplot(data.frame(x = x30), aes(x)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  geom_vline(aes(xintercept = mean(x30)), color = "blue", size = 1) +
  labs(title = "n=30", x = "resultado", y = "Frecuencia")+
  theme_minimal()

p2 <- ggplot(data.frame(x = x100), aes(x)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(x100)), color = "red", size = 1) +
  labs(title = "n=100", x = "resultado", y = "Frecuencia")+
  theme_minimal()

p3 <- ggplot(data.frame(x = x1000), aes(x)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  geom_vline(aes(xintercept = mean(x1000)), color = "red", size = 1) +
  labs(title = "n=1000", x = "resultado", y = "Frecuencia")+
  theme_minimal()

# Combinar los tres gráficos en una sola fila
(p1 | p2 | p3)


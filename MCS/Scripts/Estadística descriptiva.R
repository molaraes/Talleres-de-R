# =========================================================
# Estadística descriptiva
# Fecha: 16/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Repaso ------------------------------------------------
Nombre <- c("Teresa", "Miguel", "Haydeé", "José", "Diana") #columna 1
Edad <- c(30, 45, 60, 25, 34) #columna 2
Sexo <- factor(c("Femenino","Otro","Femenino","Masculino","Otro")) #columna 3
tabla <- data.frame(nombre=Nombre, edad=Edad, sexo= Sexo) #juntamos columnas
tabla #la vemos

#Podríamos agregar una nueva columna ya que tenemos nuestra data.frame.
tabla$profesion <- c("Trabajadora independiente","Sector público","Jubilada","Estudiante","Sector privado")
tabla

#Para acceder a valores específicos de las bases de datos podemos utilizar los corchetes. Se llama "indexing".

#Podemos hacerlo con las posiciones de las columnas y filas.
tabla[1,]
tabla[3,]
tabla[,2]
tabla[,4]
tabla[1,4]
tabla[c(1,2),]
tabla[,c(2,4)]
tabla[1:4,]

#O también estableciendo las condiciones con operadores lógicos (vistos la semana pasada).
tabla[tabla$edad > 30, ]
tabla[tabla$sexo=="Femenino",]
tabla[tabla$sexo=="Otro" & tabla$edad < 35,]
tabla[tabla$sexo=="Masculino" | tabla$edad < 30,]
tabla[tabla$sexo=="Femenino" | tabla$edad < 50,]

#Podemos exportar nuestra base de datos a csv.
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/MCS/Datos")
write.csv(tabla,"tabla-de-personas.csv")


# Paquetería --------------------------------------------------------------

install.packages("tidyverse") #Para manipular las variables
install.packages("janitor") #Para limpiar nombres y crear tablas de frecuencias
install.packages("readxl") #Para importar datos de excel
install.packages("modeest") #Para calcular moda
library(modeest)
library(janitor)
library(tidyverse) 
library(readxl)


# Análisis exploratorio ---------------------------------------------------

#Vamos a abrir la base de datos del World Justice Project de 2024, quienes calculan un índice del Estado de Derecho.
getwd()  # Muestra el directorio de trabajo actual
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/MCS/Datos") #Establece directorio de trabajo
estado_derecho <- read_excel("estado_derecho.xlsx") #Abre la base de datos

# Varios comandos para explorar nuestra base de datos
glimpse(estado_derecho)
str(estado_derecho)

#Podemos limpiar los nombres de nuestra base.
estado_derecho <- clean_names(estado_derecho)

#Vamos a renombrar la variable del índice del Estado de Derecho
estado_derecho <- estado_derecho %>% 
  rename(indice_wjp=wjp_rule_of_law_index_overall_score)

# Medidas de tendencia central --------------------------------------------

class(estado_derecho$indice_wjp)

#Media
mean(estado_derecho$indice_wjp)
#Moda
mfv(estado_derecho$region)
#Mediana
median(estado_derecho$indice_wjp)

# Medidas de dispersión ---------------------------------------------------

#Varianza
var(estado_derecho$indice_wjp)
#Desviación estándar
sd(estado_derecho$indice_wjp)
#Rango intercuartil
IQR(estado_derecho$indice_wjp)

# Tablas de frecuencias ---------------------------------------------------
#Variables dummy: convertimos como 1 aquellos valores superior a la media y 0 en caso contrario

# Calcular la media de los datos
media <- mean(estado_derecho$indice_wjp)

# Crear la variable dummy
estado_derecho <- estado_derecho %>% 
  mutate(indice_dummy = ifelse(indice_wjp >= media,"Alto","Bajo"))


class(estado_derecho$indice_dummy) #Vemos qué tipo de variable

#La podemos convertir a factor
estado_derecho <- estado_derecho %>% 
  mutate(indice_dummy = 
           factor(indice_dummy, 
                  levels=c("Bajo", "Alto")))

# Hacemos una tabla de frecuencias
estado_derecho %>% 
  tabyl(indice_dummy)



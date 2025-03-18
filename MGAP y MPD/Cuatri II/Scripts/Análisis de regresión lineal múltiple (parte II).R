# =========================================================
# Supuestos del modelo de regresión lineal múltiple
# Fecha: 17/03/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------
#Paquetes
pacman::p_load("tidyverse", "janitor", "broom")

#Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Lectura de datos

#https://www.kaggle.com/datasets/mjshri23/life-expectancy-and-socio-economic-world-bank/data

base <- read_csv("life_expectancy.csv")

#Limpiamos nombres
base <- clean_names(base)

#Quitar la notación científica
options(scipen = 999)


# Modelo de regresión lineal múltiple -------------------------------------

# Estimamos modelo múltiple: esperanza de vida explicada por gasto en salud, desempleo y gasto en educación

# Estimar el modelo
modelo_multiple <- lm(life_expectancy_world_bank~
                        health_expenditure_percent+
                        education_expenditure_percent+
                        unemployment, 
                      data=base)
summary(modelo_multiple)



# Tabla ANOVA -------------------------------------------------------------

#Tabla ANOVA
anova(modelo_multiple)

#Df (Grados de libertad)
#Residuals: 2088 (número total menos el número de parámetros estimados).

#Sum Sq (Suma de cuadrados): Variabilidad explicada por el modelo.
#Sum Sq de Residuals: Variabilidad no explicada (error).

#Mean Sq (Media de los cuadrados)
#Mean Sq = Sum Sq / Df, indica la variabilidad promedio

#F value (Estadístico F)
#Indica si la variable independiente tiene un efecto significativo en la variable dependiente.
#Se calcula como Mean Sq (modelo) / Mean Sq (residuos).

#Pr(>F) (Valor p)
#Evalúa la significancia del modelo.
#Un valor p < 0.05 indica que las variables independientes tienen un efecto significativo en life_expectancy_world_bank.



# Verificación de supuestos -----------------------------------------------

#1.Asociación lineal entre las variables dependiente e independiente.
cor.test(base$life_expectancy_world_bank, base$health_expenditure_percent, method="pearson")

cor.test(base$life_expectancy_world_bank, base$education_expenditure_percent, method="pearson")

cor.test(base$life_expectancy_world_bank, base$unemployment, method="pearson")

#2.Comportamiento normal de los errores y media 0.
# Extraer los residuos
residuos <- residuals(modelo_multiple)

# Prueba de normalidad de Shapiro-Wilk
shapiro.test(residuos)
#Ho. Hay normalidad
#Ha. No hay normalidad

# Media cero
mean(residuos) #es muy cercana a cero.

#3. Homocedasticidad: Varianza constante de los errores.
install.packages("lmtest")
library(lmtest)

# Prueba de Breusch-Pagan
bptest(modelo_multiple)

#Ho. La varianza es constante (hay homocedasticidad)
#Ha. La varianza no es constante (hay heterocedasticidad)

#4. No multicolinealidad.
install.packages("car")
library(car)

vif(modelo_multiple)

#5.Independencia de los errores

# Prueba de Durbin-Watson
dwtest(modelo_multiple)

#Ho. No hay autocorrelación en los errores
#Ha. Hay autocorrelación en los errores.

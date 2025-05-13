# =========================================================
# Criterios BIC y AIC
# Fecha: 19/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

#Paquetes
pacman::p_load("tidyverse", "smss", "MASS")

# El paquete "smss" contiene las bases de datos del libro de Agresti y Finlay "Statistical Methods for the Social Sciences".

# La documentación disponible aquí: https://cran.r-project.org/web/packages/smss/smss.pdf

# El paquete "MASS" es del libro Modern Applied Statistics with S de Venables y Ripley. Ofrece un conjunto de funciones útiles para modelos estadísticos avanzados, manipulación y simulación de datos, técnicas de reducción de dimensiones.

#Datos 
data(student.survey)

#Este archivo de datos consiste en respuestas de estudiantes de posgrado en ciencias sociales matriculados en STA 6126 en un período reciente en la Universidad de Florida.

# Variables
# CO college GPA
# GE gender
# AG age in years
# HI high school GPA (on a four-point scale)
# DH distance (in miles) of the campus from your home town
# DR distance (in miles) of the classroom from your current residence
# TV average number of hours per week that you watch TV
# SP average number of hours per week that you participate in sports or have other physical exercise
# NE number of times a week you read a newspaper
# AH number of people you know who have died from AIDS or who are HIV+
# VE whether you are a vegetarian
# PA political affiliation (d = Democrat, r = Republican, i = independent)
# PI political ideology
# RE how often you attend religious services
# AB opinion about whether abortion should be legal in the first three months of pregnancy
# AA support affirmative action
# LD belief in life after death

# Modificación variables categóricas

student.survey <- student.survey %>% 
  mutate(pi=factor(pi, 
                   levels=c("liberal", "very liberal", "slightly liberal", "moderate", "slightly conservative", "conservative", "very conservative"), ordered = FALSE))

student.survey <- student.survey %>% 
  mutate(re=factor(re, 
                   levels=c("occasionally", "never", "most weeks", "every week"), ordered = FALSE))

# Criterio BIC ------------------------------------------------------------

# Objetivo: explicar.

# El criterio BIC (Bayesian Information Criterion) o Criterio de Información Bayesiano en regresión lineal múltiple es una medida utilizada para la selección de modelos y elegir el que mejor se ajusta a los datos.

# Ventajas: 
# 1. Evalúa el ajuste del modelo (qué tan bien predice los datos).
# 2. Penaliza la complejidad del modelo (cuántos parámetros incluye).

# Menor BIC = mejor modelo.

# El algoritmo utiliza una estrategia "backward" (hacia atrás), comenzando con un modelo completo e irá eliminando variables una a una para mejorar el criterio de información.

# Supongamos hay una teoría detrás que establece que hay un efecto importante del género en el puntaje GPA.

modelo_base <- lm(co ~ ge, data=student.survey)
summary(modelo_base)

# Pero queremos ver qué pasa si le añadimos otras variables.

# Eliminamos id de la base de datos y variables con valores perdidos
student.survey.modif <- student.survey %>% 
  dplyr::select(-subj, -aa, -ld)

# Estimamos un modelo de regresión con todas las variables.
modelo_complejo <- lm(co ~.,data=student.survey.modif)
# El punto . es un operador especial en las fórmulas de R que significa "todas las demás variables"

# Vemos nuestro modelo
summary(modelo_complejo)

modelo_BIC_explicativo <-  stepAIC(
  object = modelo_complejo,      
  # Modelo inicial (incluye todas las variables)
  scope = list(lower = modelo_base, upper = modelo_complejo), 
  # Define los límites: desde el modelo más simple hasta el más complejo
  direction = "backward",    
  # Utiliza eliminación hacia atrás (comienza con todas las variables y va eliminando)
  k = log(nrow(student.survey.modif)), 
  # Usa la penalización BIC en lugar de AIC (log del número de observaciones)
  trace = TRUE              
  # Muestra cada paso del proceso de selección en la consola
)

#Modelo final (BIC=-121.70): Al final, el proceso se detiene con el modelo
#co ~ ge + hi
summary(modelo_BIC_explicativo)



# Criterio AIC ------------------------------------------------------------

# Objetivo: predecir.
# El criterio AIC (Akaike Information Criterion) en regresión múltiple también es una medida utilizada para comparar y seleccionar modelos.
# Evalúa qué tan bien se ajusta un modelo a los datos, penalizando la inclusión de demasiados parámetros, para evitar el sobreajuste.
# Al usar el AIC para la selección de modelos, se busca el modelo con el menor valor de AIC.
# No depende directamente del tamaño de la muestra, a diferencia del BIC.
# Tiende a elegir modelos más complejos, con respecto al BIC.

# Estimamos un modelo de regresión con todas las variables.
modelo_complejo <- lm(co ~.,data=student.survey.modif)
# El punto . es un operador especial en las fórmulas de R que significa "todas las demás variables"
summary(modelo_complejo)

modelo_AIC <- stepAIC(
  object = modelo_complejo, # Modelo inicial con todas las variables
  direction = "backward",   # Dirección: backward (elimina variables una a una)
  k = 2,                    # Penalización de AIC (el valor estándar)
  trace = TRUE              # Muestra el progreso paso a paso
)

#La información mostrada para cada paso incluye:

#Step: AIC= El valor AIC del modelo actual
#La fórmula del modelo actual (ej: co ~ ge + ag + hi + dh + dr + tv + sp + ne + ah + pa + pi + re)

#Una tabla con posibles cambios donde:
#La primera columna muestra la variable a eliminar (con signo -)
#<none> representa mantener el modelo sin cambios
#Df son los grados de libertad que se ganarían al eliminar esa variable
#Sum of Sq es la suma de cuadrados adicional que resultaría
#RSS es la suma de cuadrados residual resultante
#AIC es el valor AIC que tendría el modelo si se elimina esa variable

# OJO, podemos decirle a R que haga un modelo con todas las interacciones posibles (siempre y cuando el número de interacciones resultantes sea menor que el número de observaciones)

modelo_complejo <- lm(co ~ (.)^2, data = student.survey.modif)
summary(modelo_complejo)


#(.)^2 indica: todas las variables independientes y todas sus interacciones de dos en dos

#El está sobreajustado (overfitted): tenemos exactamente tantos parámetros estimados como observaciones (o incluso más). Como resultado, no quedan grados de libertad residuales.

# Veamos otra base
data(fl.crime)

fl.crime  %>% 
  dplyr::select(-County) -> fl.crime_modif

modelo_complejo <- lm(C ~ (.)^2, data = fl.crime_modif)
summary(modelo_complejo)

#Apliquemos BIC y AIC
modelo_base <- lm(C ~ I, data=fl.crime_modif)
summary(modelo_base)

modelo_complejo <- lm(C ~.,data=fl.crime_modif)
summary(modelo_complejo)

#BIC
modelo_BIC_explicativo <-  stepAIC(
  object = modelo_complejo,      
  # Modelo inicial (incluye todas las variables)
  scope = list(lower = modelo_base, upper = modelo_complejo), 
  # Define los límites: desde el modelo más simple hasta el más complejo
  direction = "backward",    
  # Utiliza eliminación hacia atrás (comienza con todas las variables y va eliminando)
  k = log(nrow(fl.crime_modif)), 
  # Usa la penalización BIC en lugar de AIC (log del número de observaciones)
  trace = TRUE              
  # Muestra cada paso del proceso de selección en la consola
)

#AIC
modelo_AIC <- stepAIC(
  object = modelo_complejo, # Modelo inicial con todas las variables
  direction = "backward",   # Dirección: backward (elimina variables una a una)
  k = 2,                    # Penalización de AIC (el valor estándar)
  trace = TRUE              # Muestra el progreso paso a paso
)


# Ejercicio ---------------------------------------------------------------

# Escoge alguna de las bases de datos de: https://cran.r-project.org/web/packages/smss/smss.pdf

# Realiza análisis exploratorio de tus datos: correlaciones, histogramas, diagramas de dispersión

# Replica el ejercicio


# =========================================================
# Repaso de regresión lineal múltiple
# Fecha: 06/05/2024
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

# Práctica de repaso ------------------------------------------------------------------

# 1. Estima un modelo de regresión lineal simple para explicar el College GPA a partir de High School GPA. Guárdalo como modelo 1.

# 2. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: edad, high school gpa, distancia de la casa al campus, horas de televisión. Guárdalo como modelo 2.

# 3. De las variables categóricas de género, ideología política y asistencia a servicios religiosos, revisa cuál es la categoría que tiene más observaciones y usa esa como categoría base. Si ya estaba así, lo puedes omitir. Tip, usa factor=() y modifica los levels=c()

# 4. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: género, edad, high school gpa, distancia de la casa al campus, horas de televisión, ideología política, asistencia a servicios religiosos. Guárdalo como modelo 3.

# 5. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: género en interacción con la edad, high school gpa, distancia de la casa al campus, horas de televisión, ideología política, asistencia a servicios religiosos. Guárdalo como modelo 4.

# 6. Interpreta los 4 modelos anteriores en términos de: efecto, significancia de los coeficientes, significancia global del modelo, R cuadrada. Escoge uno de los cuatro modelos.

# 7. Del modelo escogido: comprueba los supuestos de normalidad de los errores, homocedasticidad y multicolinealidad.

# 8. Del modelo escogido, analiza los datos atípicos. Usa cooks.distance().
# La distancia de Cooks proporciona una medida global de influencia que combina el efecto del apalancamiento (influencia potencial que una observación tiene en función de sus valores en las variables predictoras) y el residuo (residuo grande indica que el modelo no predice bien ese punto específico)
# Utiliza el siguiente umbral:
# 4/(n-k-1): 
# Donde "n" es el número de observaciones y "k" es el número de parámetros. 
# Si la distancia de cooks es mayor al umbral, es influyente la observación.

# 9. Extra 1: Supongamos hay una teoría detrás que establece que hay un efecto del género en el puntaje GPA.

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

# Vamos a valorar cuál es el mejor modelo a partir de criterio de BIC (pondera modelos más parsimoniosos)

# El criterio BIC (Bayesian Information Criterion) o Criterio de Información Bayesiano en regresión lineal múltiple es una medida estadística utilizada para la selección de modelos, similar al AIC pero con una penalización más estricta para la complejidad del modelo.

# Se selecciona el modelo con el menor valor BIC

# El algoritmo utiliza una estrategia "backward" (hacia atrás), comenzando con un modelo completo e irá eliminando variables una a una para mejorar el criterio de información.

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

# 10. Extra 2: Supongamos que todas las variables de la base de datos tienen un poder predictivo.

# Estimamos un modelo de regresión con todas las variables.
modelo_complejo <- lm(co ~.,data=student.survey.modif)
# El punto . es un operador especial en las fórmulas de R que significa "todas las demás variables"
summary(modelo_complejo)

# Elegiremos esta vez el modelo con el criterio de AIC (pondera modelos predictivos)

# El criterio AIC (Akaike Information Criterion) en regresión múltiple es una medida estadística utilizada para comparar y seleccionar modelos.

# Al usar el AIC para la selección de modelos, se busca el modelo con el menor valor de AIC, que representa el mejor equilibrio entre ajuste y parsimonia.

modelo_AIC <- stepAIC(
  object = modelo_complejo, # Modelo inicial con todas las variables
  direction = "backward",   # Dirección: backward (elimina variables una a una)
  k = 2,                    # Penalización de AIC (el valor estándar)
  trace = TRUE              # Muestra el progreso paso a paso
)

#La información mostrada para cada paso incluye:
  
#Step: AIC=valor es el valor AIC del modelo actual
#La fórmula del modelo actual (ej: co ~ ge + ag + hi + dh + dr + tv + sp + ne + ah + pa + pi + re)

#Una tabla con posibles cambios donde:
#La primera columna muestra la variable a eliminar (con signo -)
#<none> representa mantener el modelo sin cambios
#Df son los grados de libertad que se ganarían al eliminar esa variable
#Sum of Sq es la suma de cuadrados adicional que resultaría
#RSS es la suma de cuadrados residual resultante
#AIC es el valor AIC que tendría el modelo si se elimina esa variable

#Comienza con el modelo completo (AIC=-126)
#En cada paso, elimina la variable que produce el menor AIC (valor más negativo)
#Primero elimina tv (AIC baja a -128)
#Luego elimina ag (AIC baja a -129.98)
#Después elimina dh (AIC baja a -131.9)
#Sigue con ne (AIC baja a -133.77)
#Continúa con sp (AIC baja a -134.74)
#Por último elimina ah (AIC baja a -135.88)
#Se detiene porque ya no puede mejorar el AIC eliminando más variables

summary(modelo_AIC)
#El modelo final es:
  #co ~ ge + hi + dr + pa + pi + re

# 11. Extra 3. Haz lo mismo que el punto anterior pero incluyendo todas las posibles interacciones de tus variables. Tip: Se pone la fórmula de las variables entre ()^2 para generar todas sus interacciones. 


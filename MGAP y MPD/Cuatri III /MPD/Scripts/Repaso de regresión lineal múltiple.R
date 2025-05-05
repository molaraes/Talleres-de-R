# =========================================================
# Repaso de regresión lineal múltiple
# Fecha: 05/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

#Paquetes
pacman::p_load("tidyverse", "smss")

# El paquete "smss" contiene las bases de datos del libro de Agresti y Finlay "Statistical Methods for the Social Sciences".

# La documentación disponible aquí: https://cran.r-project.org/web/packages/smss/smss.pdf

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

# 6. Interpreta los 4 modelos anteriores en términos de: significancia de los coeficientes, significancia global del modelo, R cuadrada e intervalos de confianza. Escoge uno de los cuatro modelos.

# 7. Del modelo escogido: comprueba los supuestos de normalidad de los errores, homocedasticidad y multicolinealidad.

# 8. Del modelo escogido, analiza los datos atípicos. Usa cooks.distance().
# La distancia de Cooks proporciona una medida global de influencia que combina el efecto del apalancamiento (influencia potencial que una observación tiene en función de sus valores en las variables predictoras) y el residuo (residuo grande indica que el modelo no predice bien ese punto específico)
# Utiliza el siguiente umbral:
# 4/(n-k-1): 
# Donde "n" es el número de observaciones y "k" es el número de predictores. 
# Si la distancia de cooks es mayor al umbral, es influyente la observación.

# 9. Escoge una de las variables independientes de tu modelo final y grafica los valores predichos de y a partir de determinados valores de x (escogidos por ti).

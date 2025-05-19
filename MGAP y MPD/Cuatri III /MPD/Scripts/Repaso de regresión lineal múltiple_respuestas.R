# =========================================================
# Respuestas: Repaso de regresión lineal múltiple
# Fecha: 12/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

#Paquetes
pacman::p_load("tidyverse", "smss", "patchwork", "lmtest", "car")

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
options(scipen=999)

# 1. Estima un modelo de regresión lineal simple para explicar el College GPA a partir de High School GPA. Guárdalo como modelo 1.
modelo1 <- lm(co~hi, data=student.survey)
summary(modelo1)

#Ecuación del modelo 
# co=2.75+0.21hi
#En promedio, por cada punto adicional en el GPA de la secundaria, aumenta el GPA de la universidad en 0.21 puntos. Dado que el pvalor es menor al nivel de significancia, se rechaza la hipótesis de que b1 = 0, por lo que, con un nivel de confianza del 95%, podemos afirmar que existe un efecto. 
#La R cuadrada es apenas 7% es decir la variable independiente explica en 7% de la variabilidad de la variable dependiente.

# 2. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: edad, high school gpa, distancia de la casa al campus, horas de televisión. Guárdalo como modelo 2.
modelo2 <- lm(co~hi+ag+dh+tv, data=student.survey)
summary(modelo2)

#Ecuación del modelo
#co = 2.694 + 0.228×hi - 0.0006×ag - 0.000001×dh + 0.003×tv
#GPA de secundaria (hi): En promedio, por cada punto adicional en el GPA de secundaria, el GPA universitario aumenta en promedio 0.228 puntos, manteniendo constantes las demás variables. Este efecto es estadísticamente significativo (p-valor = 0.0349 < 0.05).
#Las demás variables no son significativas.

#R² = 0.08121: Las variables independientes explican el 8.12% de la variabilidad del GPA universitario.
#F-estadístico = 1.215 (p-valor = 0.3149): El modelo como conjunto no es estadísticamente significativo, ya que el p-valor > 0.05.

# 3. De las variables categóricas de género, ideología política y asistencia a servicios religiosos, revisa cuál es la categoría que tiene más observaciones y usa esa como categoría base. Si ya estaba así, lo puedes omitir. Tip, usa factor=() y modifica los levels=c()
table(student.survey$ge)
levels(student.survey$ge)

table(student.survey$pi)
levels(student.survey$pi)
student.survey <- student.survey %>% 
  mutate(pi=factor(pi, 
                   levels=c("liberal", "very liberal", "slightly liberal", "moderate", "slightly conservative", "conservative", "very conservative"), ordered = FALSE))

table(student.survey$re)
levels(student.survey$re)
student.survey <- student.survey %>% 
  mutate(re=factor(re, 
                   levels=c("occasionally", "never", "most weeks", "every week"), ordered = FALSE))

# 4. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: género, edad, high school gpa, distancia de la casa al campus, horas de televisión, ideología política, asistencia a servicios religiosos. Guárdalo como modelo 3.
modelo3 <- lm(co~ge+ag+hi+dh+tv+pi+re, data=student.survey)
summary(modelo3)

#Ecuación
# GPA universitario = 2.667 - 0.225×(gem) + 0.003×(ag) + 0.235×(hi) - 0.00002×(dh) + 0.004×(tv) + 0.197×(pivery liberal) - 0.145×(pislightly liberal) - 0.027×(pimoderate) + 0.127×(pislightly conservative) + 0.418×(piconservative) + 0.787×(pivery conservative) + 0.113×(renever) - 0.275×(remost weeks) - 0.373×(reevery week)

#Variables significativas
#Género (gem): El coeficiente de -0.225 indica que, en promedio, los estudiantes masculinos tienen un GPA universitario 0.225 puntos menor que las mujeres, manteniendo constantes las demás variables (p = 0.0194).
#GPA de secundaria (hi): Por cada punto adicional en el GPA de secundaria, el GPA universitario aumenta en promedio 0.235 puntos, manteniendo constantes las demás variables (p = 0.0312).
#Ideología política (pi): La categoría "very conservative" muestra un efecto significativo. Los estudiantes que se identifican como "muy conservadores" tienen, en promedio, un GPA universitario 0.787 puntos mayor que los estudiantes "liberales" (categoría de referencia), manteniendo constantes las demás variables (p = 0.0122).
#Asitencia a servicios religiosos (re): Los estudiantes que asisten a servicios religiosos "every week" tienen, en promedio, un GPA universitario 0.373 puntos menor que aquellos que asisten "occasionally" (categoría de referencia), manteniendo constantes las demás variables (p = 0.0463).

#R² = 0.3968: Las variables incluidas en el modelo explican aproximadamente el 39.7% de la variabilidad en el GPA universitario, lo cual representa una mejora sustancial respecto al modelo anterior (8.12%).
#F-estadístico = 2.115 (p-valor = 0.02927): El modelo como conjunto es estadísticamente significativo a un nivel de significancia de 0.05.

# 5. Estima un modelo de regresión lineal múltiple para explicar el College GPA a partir de: género en interacción con la edad, high school gpa, distancia de la casa al campus, horas de televisión, ideología política, asistencia a servicios religiosos. Guárdalo como modelo 4.
modelo4 <- lm(co~ge*ag+ hi+dh+tv+pi+re, data=student.survey)
summary(modelo4)

#Ecuación del modelo
#GPA universitario = 2.538 - 0.011×(gem) + 0.008×(ag) + 0.229×(hi) - 0.00002×(dh) + 0.004×(tv) + 0.189×(pivery liberal) - 0.182×(pislightly liberal) - 0.032×(pimoderate) + 0.098×(pislightly conservative) + 0.426×(piconservative) + 0.759×(pivery conservative) + 0.132×(renever) - 0.273×(remost weeks) - 0.352×(reevery week) - 0.007×(gem×ag)

#GPA de secundaria (hi): Por cada punto adicional en el GPA de secundaria, el GPA universitario aumenta en promedio 0.229 puntos, manteniendo constantes las demás variables (p = 0.0378). Esto confirma que el rendimiento académico previo es un predictor significativo del rendimiento universitario.

#Ideología política - muy conservador (pivery conservative): Los estudiantes que se identifican como "muy conservadores" tienen, en promedio, un GPA universitario 0.759 puntos mayor que los estudiantes "liberales" (categoría de referencia), manteniendo constantes las demás variables (p = 0.0172). Este es un efecto considerable y sugiere una relación entre la orientación política muy conservadora y un mejor rendimiento académico en este conjunto de datos.

#R² = 0.402: Las variables incluidas en el modelo explican aproximadamente el 40.2% de la variabilidad en el GPA universitario. Esto representa una ligera mejora respecto al modelo anterior sin la interacción (39.68%), lo que sugiere que la interacción entre género y edad aporta muy poco poder explicativo adicional.

#F-estadístico = 1.972 (p-valor = 0.04096): El modelo como conjunto es estadísticamente significativo al nivel de significancia de 0.05, aunque el p-valor está bastante cerca del umbral crítico. 

# 6. Interpreta los 4 modelos anteriores en términos de: efecto, significancia de los coeficientes, significancia global del modelo, R cuadrada. Escoge uno de los cuatro modelos.
# (Interpretados arriba).

# 7. Del modelo escogido: comprueba los supuestos de normalidad de los errores, homocedasticidad y multicolinealidad.

#Normalidad de los errores
#creamos errores
student.survey$res <- residuals(modelo3)

# Crear el QQ-plot
p1 <- ggplot(student.survey, aes(sample = res)) +
  stat_qq() + #Dibuja los puntos comparando los cuantiles teóricos con observados.
  stat_qq_line() + #Línea de referencia
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Crear el histograma
p2 <- ggplot(student.survey, aes(x = res)) +
  geom_histogram(bins = 10, fill = "gray", color = "black") +
  labs(title = "Histogram of res",
       x = "res",
       y = "Frequency") +
  theme_minimal()

# Crear el boxplot
p3 <- ggplot(student.survey, aes(y = res)) +
  geom_boxplot(fill = "gray") +
  labs(title = "Boxplot of res",
       y = "res") +
  theme_minimal() 

# Combinar los gráficos con patchwork
p1 | p2 | p3

#Ho: hay normalidad
#Ha: no hay normalidad
ks.test(student.survey$res, "pnorm", mean = mean(student.survey$res), sd = sd(student.survey$res))
#No rechazamos la nula

#Homocedasticidad
#Creamos valores predichos
student.survey$pred <- predict(modelo3)
#Creamos errores estimados estandarizados
student.survey$res_estandarizados <- rstandard(modelo3)


p1 <- ggplot(data = student.survey, aes(x = pred, y = res)) +
  geom_point() +
  geom_hline(yintercept = c(-2, -1, 0, 1, 2), linetype = "dashed", color = "gray") +
  labs(
    x = "Y pronosticados(ajustados)",
    y = "Errores"
  ) +
  ylim(-3, 3) +
  theme_minimal() 

p2 <- ggplot(data = student.survey, aes(x = pred, y = res_estandarizados)) +
  geom_point() +
  geom_hline(yintercept = c(-2, -1, 0, 1, 2), linetype = "dashed", color = "gray") +
  labs(
    x = "Y pronosticados(ajustados)",
    y = "Errores estandarizados"
  ) +
  ylim(-3, 3) +
  theme_minimal() 


# Combinar los gráficos
p1 + p2 + 
  plot_annotation(
    title = "Verificación supuesto homocedasticidad: Varianza constante de los errores",
    theme = theme(plot.title = element_text(size = 14))
  )

#Prueba de hipótesis
bptest(modelo3)
#No se rechaza la nula, no podemos rechazar que la varianza es constante.

#Multicolinealidad
vif(modelo3)

# 8. Del modelo escogido, analiza los datos atípicos. Usa cooks.distance().
# La distancia de Cooks proporciona una medida global de influencia que combina el efecto del apalancamiento (influencia potencial que una observación tiene en función de sus valores en las variables predictoras) y el residuo (residuo grande indica que el modelo no predice bien ese punto específico)

influenceIndexPlot(modelo3, vars="Cook")

# Utiliza el siguiente umbral:
# 4/(n-k-1): 
# Donde "n" es el número de observaciones y "k" es el número de parámetros. 
n <- nrow(student.survey)  # Número de observaciones
k <- length(coef(modelo3)) - 1  # Número de coeficientes sin intercepto
umbral_cook <- 4/(n-k-1)

# Calcular la distancia de Cook
dist_cook <- cooks.distance(modelo3)

# Identificar observaciones influyentes
obs_influyentes <- which(dist_cook > umbral_cook)


student.survey %>% 
  dplyr::filter(subj!=11 &
                  subj!=45 &
                  subj!=47 &
                  subj!=56) -> base_modif

modelo3_1 <- lm(co~ge+ag+hi+dh+tv+pi+re, 
                data=base_modif)
summary(modelo3)
summary(modelo3_1)

# 9. Escoge una de las variables independientes de tu modelo final y grafica los valores predichos de y a partir de determinados valores de x (escogidos por ti).

nuevos_datos <- data.frame(
  hi = c(1, 2, 3, 4, 5),
  ge = "f",  
  ag = mean(student.survey$ag),  
  dh = mean(student.survey$dh),  
  tv = mean(student.survey$tv), 
  pi = "liberal", 
  re = "occasionally" 
)

#Con la función predict voy a ver los valores de mi variable dependiente, dados ciertos valores de mi variable independiente
predicciones <- predict(modelo3, 
        newdata = nuevos_datos, 
        interval = "confidence", 
        level=0.95)

resultados <- cbind(nuevos_datos["hi"], predicciones)


#nuevos_datos["hi"] selecciona solo la columna hi del data frame,
#cbind(...) combina esa columna con el resultado de predict(),
#El objeto final resultados contiene:
#La variable independiente hi,
#La predicción de la variable dependiente,
#Los límites inferior y superior del intervalo de confianza.
resultados %>% 
ggplot(aes(x = hi, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # Intervalo de confianza
  labs(title = "Valores predichos de la variable dependiente según hi\nmanteniendo las demás constantes",
       x = "Valores de hi",
       y = "Valores predichos de co") +
  theme_minimal()


# =========================================================
# Modelo de regresión lineal múltiple
# Fecha: 10/03/2024
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

#Podemos ver el rendimiento de nuestro modelo
install.packages("performance")
install.packages("see")
install.packages("patchwork")
install.packages("qqplotr")
library(performance)
library(see)
library(patchwork)
library(qqplotr)

#El paquete performance en R es una herramienta muy útil para evaluar y comparar modelos estadísticos. 1) Calcula índices de ajuste como R², AIC, BIC, RMSE. 2) Verifica supuestos de los modelos (normalidad, homocedasticidad, multicolinealidad) 3. Detecta valores atípicos e influyentes

result1<- check_model(modelo_multiple)
plot(result1)

check_outliers(modelo_multiple, method="cook")


#1. La distribución de los datos observados (línea verde) y los datos predichos por el modelo (línea azul) muestran cierta discrepancia.

#2. La línea de referencia no es plana ni horizontal como debería ser. Se observa un patrón curvilíneo en los residuos, especialmente para valores ajustados entre 65 y 75 años.

#3. Los residuos parecen ser más variables en ciertos rangos de valores ajustados.

#4. Se identifican varios puntos potencialmente influyentes (1583, 1458, 237, 731, 440). Estos puntos están cerca del límite o fuera de los contornos que representan la distancia de Cook. La distancia de Cook es una medida que combina el leverage (influencia potencial basada en valores extremos en las variables independientes) y los residuos. Mide cuánto cambiarían los coeficientes estimados si se eliminara una observación específica. El umbral típico para la distancia de Cook suele ser 4/n (donde n es el número de observaciones) o 1. En este caso, parece que se está utilizando un umbral de 0.8 (como se ve en las líneas punteadas del gráfico). Este umbral probablemente fue escogido porque representa un balance entre ser demasiado restrictivo (lo que marcaría muchas observaciones como influyentes) y ser demasiado permisivo (lo que podría pasar por alto puntos verdaderamente problemáticos).
#Así como Cook, hay otras distancias: dffits y dfbetas.

#5. El Factor de Inflación de la Varianza (VIF) para todas las variables independientes está por debajo de 5. Esto es positivo e indica que no hay problemas significativos de multicolinealidad entre las variables independientes.

#6. El gráfico Q-Q muestra desviaciones considerables de la línea diagonal, especialmente en los extremos.
#Esto indica que los residuos no siguen una distribución normal, violando otro supuesto clave del modelo de regresión lineal.

#Posibles soluciones: 
#Transformación de variables: Considerar transformaciones (logarítmica, raíz cuadrada, etc.) para la variable dependiente o las independientes para abordar la no linealidad.
#Revisión de las observaciones influyentes: Examinar los casos 1583, 1458, 237, 731 y 440 para determinar si son outliers legítimos o errores de datos. Considerar realizar análisis de sensibilidad excluyendo estos puntos.
#Variables adicionales: Considerar la inclusión de variables adicionales que puedan estar causando la falta de ajuste actual.


#Para ver la importancia de mis variables independientes
install.packages("relaimpo")
library(relaimpo)

importancia <- calc.relimp(modelo_multiple, type="lmg",rela=T)
importancia
sort(importancia$lmg, decreasing=TRUE) # relative importance

#calc.relimp() es una función del paquete relaimpo en R, que se utiliza para calcular la importancia relativa de predictores en modelos de regresión.
#type="lmg" especifica el método de cálculo de importancia relativa. El método "lmg" (también conocido como método de Lindeman, Merenda y Gold) descompone el R² en contribuciones no negativas para cada predictor. Este método maneja bien la multicolinealidad al promediar las contribuciones secuenciales de cada predictor sobre todos los posibles órdenes de los predictores.
#rela=T (abreviatura de rela=TRUE) indica que los valores de importancia deben normalizarse para que sumen 1 (o 100%), lo que facilita la interpretación de la importancia relativa de cada variable.


# Variables categóricas como independientes -------------------------------
modelo_cat <- lm(life_expectancy_world_bank~
                        health_expenditure_percent+
                        education_expenditure_percent+
                        unemployment+
                   income_group, 
                      data=base)

summary(modelo_cat)

base <- base %>% 
  mutate(ingreso=case_when(income_group=="High income"~"Alto",
                           income_group=="Low income"~"Bajo",
                           income_group=="Lower middle income"~"Bajo",
                           income_group=="Upper middle income"~"Alto"
  ))

table(base$ingreso)

base <- base %>% 
  mutate(ingreso=factor(ingreso, levels=c("Bajo", "Alto")))

# Interacciones -----------------------------------------------------------

#Interacciones
#Creamos una variable nueva si es de high income o los demás
base <- base %>% 
  mutate(dummy_high=ifelse(income_group!="High income", "Medio y bajo", "Alto"))

modelo_inter <- lm(life_expectancy_world_bank~
                   health_expenditure_percent*dummy_high+
                   education_expenditure_percent+
                   unemployment, 
                 data=base)

summary(modelo_inter)

#Gráfico
install.packages("sjPlot")
library(sjPlot)

plot_model(modelo_inter, type = "pred", 
           terms = c("health_expenditure_percent", "dummy_high"))

#¿Qué podemos decir del gráfico?

# Modelo con betas estandarizadas -----------------------------------------
install.packages("lm.beta")
library(lm.beta)
modelo_beta <- lm.beta(modelo_multiple) 
#estandariza los coeficientes para leerlos en unidades de desviación estándar
summary(modelo_beta)

#Grafiquemos los cuatro modelos
# Extraer los coeficientes de cada modelo y combinarlos en una tabla tidy
coeficientes_tidy <- bind_rows(
  tidy(modelo_multiple) %>% mutate(modelo = "Múltiple"),
  tidy(modelo_cat) %>% mutate(modelo = "Categórico"),
  tidy(modelo_inter) %>% mutate(modelo = "Interacción"),
  tidy(modelo_beta) %>% mutate(modelo = "Beta")
)

# Opcional: eliminar interceptos
coeficientes_tidy <- coeficientes_tidy %>%
  filter(term != "(Intercept)")

# Graficar los coeficientes con sus intervalos de confianza
ggplot(coeficientes_tidy, aes(x = term, y = estimate, color = modelo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error*1.96, 
                    ymax = estimate + std.error*1.96),
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +  # Para que los términos aparezcan en el eje vertical
  labs(title = "Comparación de coeficientes entre modelos",
       x = "Variables",
       y = "Coeficientes estimados",
       color = "Modelo") +
  theme_minimal()

# =========================================================
# Modelo de regresión lineal simple
# Fecha: 24/02/2024
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

# Análisis de regresión lineal simple -------------------------------------

# Vamos a explicar la esperanza de vida a partir del gasto en salud como porcentaje del PIB

# Hacemos un análisis previo de nuestras variables

# Variable independiente: gasto en salud
base %>% 
  filter(!is.na(health_expenditure_percent)) %>% 
  ggplot(aes(x=health_expenditure_percent))+
  geom_histogram()+
  theme_minimal()

# Variable dependiente: esperanza de vida
base %>% 
  filter(!is.na(life_expectancy_world_bank)) %>% 
  ggplot(aes(x=life_expectancy_world_bank))+
  geom_histogram()+
  theme_minimal()

#¿Qué países tienen la esperanza de vida menor?
base %>% #base de datos
  arrange(life_expectancy_world_bank) %>% #orden ascendente
  select(country_name, year, life_expectancy_world_bank) %>% 
  #selecciono lo que me interesa
  head() #primeras 6 observaciones

#¿Y el gasto en salud?
base %>% #base de datos
  arrange(health_expenditure_percent) %>% #orden ascendente
  select(country_name, year, health_expenditure_percent) %>% 
  #selecciono lo que me interesa
  head() #primeras 6 observaciones

# Graficamos
base %>% #mi base 
  ggplot(aes(x=health_expenditure_percent, #independ
             y=life_expectancy_world_bank))+ #dep
  geom_point()+ #dispersión
  geom_smooth(method="lm")+ #recta
  theme_minimal()+#tema
  labs(title="Relación entre esperanza de vida y gasto en salud a nivel mundial, 2001-2019",
       x= "Gasto en salud (% del PIB)",
       y="Esperanza de vida (en años)",
       caption="Elaboración propia con datos del Banco Mundial")

# ¿Y será significativa la correlación?
cor.test(base$life_expectancy_world_bank, base$health_expenditure_percent, 
         method = "pearson")

# Ahora sí hacemos la regresión
modelo <- lm(life_expectancy_world_bank~ 
               health_expenditure_percent, 
             data=base) #estimo

summary(modelo) #veo resultado

#Residuales: son las diferencias entre los valores observados y los valores predichos por el modelo. El output muestra la distribución.

#Estimate: Los coeficientes estimados del modelo

#(Intercept): El valor predicho de life_expectancy_world_bank cuando health_expenditure_percent es 0 (62.46 años).

#health_expenditure_percent: Indica que por cada aumento de 1% en el gasto en salud, la esperanza de vida aumenta en 1.18 años.

#Std. Error: Error estándar de la estimación del coeficiente.

#t value: Estadístico t para evaluar la significancia del coeficiente.

#Pr(>|t|): Valor p, indica la probabilidad de que el coeficiente sea 0.

#Ambos coeficientes son altamente significativos (p < 0.001), lo que sugiere que el gasto en salud está relacionado con la esperanza de vida.

#Residual standard error: Indica cuánto se desvían en promedio los valores observados de los valores predichos.

#Degrees of freedom (DF): 2974, indica el número de datos menos el número de parámetros estimados.

#Multiple R-squared: Explica qué porcentaje de la variabilidad en la esperanza de vida es explicado por el gasto en salud (10.65%).

#Adjusted R-squared: Similar, pero ajustado por el número de predictores en el modelo.

#Prueba F: Prueba la hipótesis nula de que todos los coeficientes son 0 (excepto la constante).

confint(modelo) #veo el intervalo de confianza de mis coeficientes al 95%

#Tabla ANOVA
anova(modelo)

#Df (Grados de libertad)
#health_expenditure_percent: 1 (porque es una sola variable independiente).
#Residuals: 2974 (número total de observaciones menos el número de parámetros estimados).

#Sum Sq (Suma de cuadrados)
#Sum Sq de health_expenditure_percent: Variabilidad explicada por el modelo.
#Sum Sq de Residuals: Variabilidad no explicada (error).

#Mean Sq (Media de los cuadrados)
#Mean Sq = Sum Sq / Df, indica la variabilidad promedio

#F value (Estadístico F)
#Indica si la variable independiente tiene un efecto significativo en la variable dependiente.
#Se calcula como Mean Sq (modelo) / Mean Sq (residuos).

#Pr(>F) (Valor p)
#Evalúa la significancia del modelo.
#Un valor p < 0.05 indica que health_expenditure_percent tiene un efecto significativo en life_expectancy_world_bank.


# Calcular R cuadrado
SS_modelo <- 27254  # Suma de cuadrados explicada por la variable independiente
SS_residuos <- 228553  # Suma de cuadrados de los residuos
SS_total <- SS_modelo + SS_residuos  # Suma de cuadrados total

# Calcular R cuadrado
R2 <- SS_modelo / SS_total

#¿Cuáles son los valores de la variable dependiente a partir de ciertos valores de la variable independiente?

# Valores específicos:
# Creo un nuevo data frame con los valores de la variable independiente
nuevos_datos <- data.frame(health_expenditure_percent = c(0.5, 0.10, 0.25))

#O para todos los valores de x
#Los guardo en una columna en el marco de datos original
base$salud_pred <- predict(modelo, newdata = base)

# Crear la gráfica con puntos y agregar la línea de ajuste (es lo mismo que hicimos al principio)

ggplot(data = base, aes(x = health_expenditure_percent, y = life_expectancy_world_bank)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  # Puntos en azul
  geom_line(aes(y = salud_pred), color = "steelblue", size = 1) +  # Línea de ajuste usando predicciones
  labs(title="Relación entre esperanza de vida y gasto en salud a nivel mundial\n2001-2019",
       x= "Gasto en salud (% del PIB)",
       y="Esperanza de vida (en años)",
       caption="Elaboración propia con datos del Banco Mundial")+
  theme_light()  # Tema con diseño sencillo

# Podemos graficar el coeficiente con su respectivo intervalo de confianza

# Obtenemos el resumen de los coeficientes con sus intervalos de confianza
coeficientes <- tidy(modelo, conf.int = TRUE)

# Graficar los coeficientes con intervalos de confianza
ggplot(coeficientes, aes(x = term, y = estimate)) +
  geom_point(size = 1.5, color = "blue") +  # Punto para la estimación
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Barras de error
  labs(title = "Resultados del modelo",
       x = "Términos del Modelo",
       y = "Estimación del Coeficiente") +
  theme_minimal()


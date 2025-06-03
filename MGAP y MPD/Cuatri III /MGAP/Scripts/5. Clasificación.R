# =========================================================
# Clasificación y modelos de regresión logística
# Fecha: 03/06/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

#Modelos explicativos:
#"¿Cómo afecta la edad a la probabilidad de sufrir discriminación?"
#Problema de clasificación: 
#"¿Podemos predecir correctamente quién sufrirá discriminación basándonos en edad, educación, etc.?"


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "caret", "forcats", "janitor", "MASS", "pROC")

#Abrimos base de datos
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MPD/Datos")

base <- read_csv("Student Depression Dataset.csv") %>% 
  dplyr::select(-id)

base <- clean_names(base)

#Descriptor de archivos
#https://www.kaggle.com/datasets/hopesb/student-depression-dataset

#Dejamos solamente las filas con datos completos
colSums(is.na(base))
base <- base %>% filter(complete.cases(.))

# Preparación de variables cualitativas -------------------------------------------

#Variable Gender:

table(base$gender)
base$gender <- factor(base$gender, levels = c("Male", "Female"))

#Variable City:

table(base$city)
#Es una variable categórica con más de 10 niveles
#Vamos a reagrupar niveles: aquellas que tengan menos del 4% las ponemos en "otras"
#El % se puede definir por ensayo y error para tener menos de 10 grupos
#Usamos la función fct_lump_prop() del paquete forcats para reducir la alta dimensionalidad de la variable City 

base <- base %>%
  mutate(city_reagrupada = fct_lump_prop(city, prop = 0.04, other_level = "Otros"))

# Ver resultado
table(base$city_reagrupada)

base$city_reagrupada <- factor(base$city_reagrupada,levels = c("Otros","Hyderabad", "Kalyan", "Lucknow", "Srinagar", "Thane", "Vasai-Virar"))


#Observación --> Se pueden agrupar por otras consideraciones dependiendo de las características de las variables. Por ejemplo --> City se pudo haber reagrupado por cercanía territorial. O por alguna otra consideración. Es importante describir el criterio empleado para agrupar

#Variable Profession:
table(base$profession)

#Está muy cargado el nivel "student" de la variable profession 
#Aunque reagrupemos, el nivel "Otros" tendrá menos del 1%. 
#Es mejor quitarla. 

base <- base %>% dplyr::select(-profession)

#Variable Sleep.Duration: 
table(base$sleep_duration)

#Es una variable ordinal:
base$sleep_duration <- factor(base$sleep_duration,
                                         levels = c("Less than 5 hours",
                                                    "5-6 hours", 
                                                    "7-8 hours", 
                                                    "More than 8 hours",
                                                    "Others"))

#Variable Dietary.Habits: 
table(base$dietary_habits)
#Es una variable ordinal:
base$dietary_habits <- factor(base$dietary_habits,
                                         levels = c("Unhealthy",
                                                    "Moderate", 
                                                    "Healthy",
                                                    "Others"))


#Variable Degree: 
table(base$degree)

#Tiene muchos niveles, hay que reagrupar:
base <- base %>%
  mutate(degree_agrupada = fct_lump_prop(degree, prop = 0.05, other_level = "Otros"))

#Revisar los niveles
table(base$degree_agrupada)

base$degree_agrupada <- factor(base$degree_agrupada,
                                            levels = c("Otros",
                                                       "B.Arch", 
                                                       "B.Com", 
                                                       "B.Ed",
                                                       "BCA",
                                                       "Class 12"))

#Variable suicidal thoughts
table(base$have_you_ever_had_suicidal_thoughts)

#Es una variable binaria
base$have_you_ever_had_suicidal_thoughts <- factor(base$have_you_ever_had_suicidal_thoughts,
                                                               levels = c("Yes",
                                                                          "No"))

#Variable Family.History.of.Mental.Illness: 
table(base$family_history_of_mental_illness)

base$family_history_of_mental_illness <- factor(base$family_history_of_mental_illness,
                                                           levels = c("No",
                                                                      "Yes"))

#Quitamos variables city y degree
base <- base %>% 
  dplyr::select(-city, -degree)

# Modelo de regresión logística -------------------------------------------

modelo_complejo <- glm(formula=depression~.,
              family=binomial(link="logit"),
              data=base)

summary(modelo_complejo)

# Buscar con el criterio de AIC (pondera modelos predictivos)
modelo_aic <- stepAIC(
  object = modelo_complejo,      
  direction = "backward",   
  k = 2, 
  trace = TRUE             
)
summary(modelo_aic)


# Tabla clasificación -----------------------------------------------------

# PASO 1: Obtener las probabilidades predichas
# Las probabilidades están entre 0 y 1
probabilidades_predichas <- predict(modelo_aic, type = "response")

# PASO 2: Convertir probabilidades a clasificaciones binarias usando umbral 0.5
umbral <- 0.5
clasificaciones_predichas <- ifelse(probabilidades_predichas >= umbral, 1, 0)

# PASO 3: Comparar con los valores reales observados
valores_observados <- base$depression

# PASO 4: Construir la matriz de confusión/tabla de clasificación

# Convertir a factores si es necesario
valores_observados <- factor(valores_observados)
clasificaciones_predichas <- factor(clasificaciones_predichas)

# Crear matriz de confusión detallada
matriz_confusion <- confusionMatrix(clasificaciones_predichas, 
                                    valores_observados, 
                                    positive = "1")  

matriz_confusion

#Del total de estudiantes, ¿qué % fueron clasificados correctamente?
#Exactitud: Proporción de predicciones correctas del total
matriz_confusion$overall["Accuracy"] 

#De los estudiantes que predijo como que sufren depresión, ¿qué % realmente sufren?
#Precision:Proporción de verdaderos positivos entre las predicciones positivas
matriz_confusion$byClass["Precision"]

#De todos los estudiantes que SÍ sufren depresión, ¿qué porcentaje fue correctamente identificado por el modelo?
#Sensibilidad: La capacidad del modelo para detectar correctamente los casos positivos.
matriz_confusion$byClass["Sensitivity"]

#De todos los estudiantes que NO sufren depresión, ¿qué porcentaje fue correctamente identificado por el modelo?
#Especificidad: proporción de casos negativos reales identificados correctamente
matriz_confusion$byClass["Specificity"]

# Curva ROC ---------------------------------------------------------------
#La curva ROC (Receiver Operating Characteristic) es una herramienta gráfica fundamental para evaluar el rendimiento de un modelo de regresión logística, especialmente útil para problemas de clasificación binaria.

#La curva ROC grafica la relación entre:
#Eje Y: Sensibilidad (Tasa de Verdaderos Positivos) = TP / (TP + FN)
#Eje X: 1 - Especificidad (Tasa de Falsos Positivos) = FP / (FP + TN)

#¿Cómo se construye?
#El modelo genera probabilidades para cada observación (0 a 1)
#Se prueban múltiples puntos de corte (0.1, 0.2, 0.3, ..., 0.9)
#Para cada punto de corte se calcula sensibilidad y especificidad
#Se grafican todos estos puntos formando la curva

#Interpretación
#Curva ideal: Se acerca a la esquina superior izquierda (sensibilidad alta, especificidad alta)
#Línea diagonal: Representa un modelo aleatorio sin poder predictivo

#Área bajo la curva (AUC):
#AUC = 1.0: Modelo perfecto
#AUC = 0.9-1.0: Excelente
#AUC = 0.8-0.9: Bueno
#AUC = 0.7-0.8: Aceptable
#AUC = 0.6-0.7: Pobre
#AUC = 0.5: Sin valor predictivo (equivale a lanzar una moneda)
#AUC < 0.5: Peor que aleatorio

# Crear la curva ROC usando las probabilidades del conjunto de prueba
roc_curve <- roc(base$depression, probabilidades_predichas)

# Graficar la curva ROC
plot(roc_curve, 
     main = "Curva ROC - Modelo de Depresión",
     col = "blue",
     lwd = 2,
     print.auc = TRUE,  # Muestra el AUC en el gráfico
     auc.polygon = TRUE,  # Rellena el área bajo la curva
     auc.polygon.col = "lightblue")

# Agregar línea de referencia (modelo aleatorio)
abline(a = 0, b = 1, lty = 2, col = "red", lwd = 2)

# Obtener el valor de AUC
auc_value <- auc(roc_curve)
#El modelo tiene una probabilidad del 92.2% de clasificar correctamente a una persona con depresión como "positiva" y a una persona sin depresión como "negativa" cuando se comparan pares aleatorios.

#Forma de la curva: Se acerca mucho a la esquina superior izquierda, lo que indica:

#Alta sensibilidad: Detecta bien los casos de depresión
#Alta especificidad: Evita bien los falsos positivos

#Distancia de la línea diagonal roja: Tu curva está muy separada de la línea de referencia (clasificador aleatorio), confirmando que tu modelo es mucho mejor que una predicción al azar.


# EXTRA: DATOS DE ENTRENAMIENTO Y PRUEBA -----------------------------------------------------

#Entrenamiento del modelo: El conjunto de entrenamiento (generalmente 70-80% de los datos) se utiliza para que el modelo de regresión logística aprenda los patrones y relaciones entre las variables independientes y la variable objetivo.

#Evaluación independiente: El conjunto de prueba (20-30% restante) permite evaluar el rendimiento del modelo en datos que no ha visto durante el entrenamiento, proporcionando una estimación más realista de su capacidad de generalización.

#Prevención del sobreajuste: Esta división ayuda a detectar si el modelo está memorizando los datos de entrenamiento (sobreajuste) en lugar de aprender patrones generales.

#1. Fijamos número semilla
set.seed(123456) 

#2. Creamos un vector con los índices de los datos de entrenamiento:
indice <- createDataPartition(y=factor(base$depression), p = 0.7, list = FALSE)  
# 70% entrenamiento

indice

#El índice que genera createDataPartition() es un vector de posiciones (filas) que indica cuáles observaciones del conjunto original base deben ir al conjunto de entrenamiento.

#List=FALSE nos devuelve un vector de índices.

#3. Dividimos los datos
entrenamiento <- base[indice, ]
prueba <- base[-indice, ]

#4. Verificamos la distribución de ceros y unos

# Proporción de ceros y unos en datos originales
table(base$depression) / nrow(base)  

# Proporción de ceros y unos en datos de entrenamiento
table(entrenamiento$depression) / nrow(entrenamiento)  

# Proporción de ceros y unos datos de prueba
table(prueba$depression) / nrow(prueba)


#5. Ajustamos el modelo de regresión logística en los datos de entrenamiento
modelo_train <- glm(formula=depression~age+academic_pressure+cgpa+study_satisfaction+sleep_duration+dietary_habits+have_you_ever_had_suicidal_thoughts+work_study_hours+financial_stress+family_history_of_mental_illness+city_reagrupada, 
                    data = entrenamiento, 
                    family = binomial(link="logit"))

# Vemos resumen del modelo (opcional)
summary(modelo_train)


#6. Calculamos probabilidades en el conjunto de prueba
probabilidades <- predict(modelo_train, newdata = prueba, type = "response")
probabilidades

#7. Convertimos probabilidades
predicciones <- ifelse(probabilidades >= 0.5, 1, 0) 
predicciones

#8. Creamos la matriz de confusión
# Es una tabla que muestra la cantidad de predicciones correctas e incorrectas organizadas por clase, comparando las predicciones del modelo con los valores reales.

matriz_caret <- confusionMatrix(as.factor(predicciones), # Predicciones
                                as.factor(prueba$depression), # Valores reales
                                positive = "1") # Evento de interés


#Del total de estudiantes, ¿qué % fueron clasificados correctamente?
#Exactitud: Proporción de predicciones correctas del total
matriz_caret$overall["Accuracy"] 

#De los estudiantes que predijo como que sufren depresión, ¿qué % realmente sufren?
#Precision:Proporción de verdaderos positivos entre las predicciones positivas
matriz_caret$byClass["Precision"]

#De todos los estudiantes que SÍ sufren depresión, ¿qué porcentaje fue correctamente identificado por el modelo?
#Sensibilidad: La capacidad del modelo para detectar correctamente los casos positivos.
matriz_caret$byClass["Sensitivity"]

#De todos los estudiantes que NO sufren depresión, ¿qué porcentaje fue correctamente identificado por el modelo?
#Especificidad: proporción de casos negativos reales identificados correctamente
matriz_caret$byClass["Specificity"]



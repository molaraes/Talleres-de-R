# =========================================================
# Árboles de clasificación
# Fecha: 23/06/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "caret", "forcats", "janitor", "MASS", "class", "reshape2", "rpart", "tree", "rpart.plot")

#Abrimos base de datos
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MPD/Datos")

base <- read_csv("Student Depression Dataset.csv") %>% 
  dplyr::select(-id)

base <- clean_names(base)

#Dejamos solamente las filas con datos completos
colSums(is.na(base))
base <- base %>% filter(complete.cases(.))

#Descriptor de archivos
#https://www.kaggle.com/datasets/hopesb/student-depression-dataset

# Preparación de la base  -------------------------------------------

#Generamos un marco de datos para el modelo de árbol de clasificación
base_modif <- base %>% 
dplyr::select(c(depression, academic_pressure, study_satisfaction, gender, city, sleep_duration, dietary_habits))

# Hacemos las modificaciones a las variables

#City

base_modif <- base_modif   %>%
  mutate(city_reagrupada = fct_lump_prop(city, prop = 0.04, other_level = "Otros")) %>% 
  dplyr::select(-city)

table(base_modif$city_reagrupada)

base_modif$city_reagrupada <- factor(base_modif$city_reagrupada, levels = c("Otros","Hyderabad", "Kalyan", "Lucknow", "Srinagar", "Thane", "Vasai-Virar"))


# Convertimos variables nominales a factor

table(base_modif$gender)
base_modif$gender <- factor(base_modif$gender, levels = c("Male", "Female"))

# Convertimos variables ordinales a factor con niveles ordenados

base_modif$sleep_duration <- factor(base_modif$sleep_duration ,levels = c("Less than 5 hours","5-6 hours", "7-8 hours", "More than 8 hours","Others"))


base_modif$dietary_habits <- factor(base_modif$dietary_habits, levels = c("Unhealthy", "Moderate", "Healthy","Others"))


# Convertir la variable respuesta a factor binario
base_modif$depression <- factor(base_modif$depression, levels = c(0,1))


# Árbol -----------------------------------------------------------

#Ahora usamos la función tree() para ajustar un árbol de clasificación

tree.depression <- tree(depression ~ ., base_modif)
summary(tree.depression)

#De todas las variables disponibles, el algoritmo solo seleccionó academic_pressure para construir el árbol
#Esto indica que esta variable es la más informativa para predecir depresión según el criterio del algoritmo
#El árbol tiene 3 nodos terminales (hojas): esto significa que clasifica las observaciones en 3 grupos finales basándose en diferentes niveles de presión académica
#Valores más bajos de la devianza indican mejor ajuste (desviación total residual/grados de libertad)

#Usamos la función plot() para mostrar la estructura de árbol y  la función text() para mostrar las etiquetas de los  nodos. El argumento pretty = 0 le indica a R que  incluya los nombres de categoría para cualquier predictor cualitativo, en lugar de simplemente mostraruna letra para cada categoría.

plot(tree.depression)
text(tree.depression, pretty=0)

#Si solo escribimos el nombre del objeto del árbol, R imprime  la salida correspondiente a cada rama del árbol. R muestra el criterio de división, el número de  observaciones en esa rama, la desviación, la predicción general  para la rama (Sí o No) y la fracción de observaciones en esa rama que toman valores de Sí y No. Las ramas que conducen a los nodos  terminales se indican con asteriscos.

tree.depression



# Evaluando el modelo -----------------------------------------------------

#Para evaluar correctamente el rendimiento de un árbol de clasificación en estos datos, debemos estimar el error de prueba. Dividimos las observaciones en un conjunto de entrenamiento y un conjunto de prueba, construimos el árbol utilizando el conjunto de entrenamiento y evaluamos su rendimiento en los datos de prueba. 

#La función predict() se puede utilizar para este propósito. En el caso de un árbol de clasificación, el argumento tipo = "clase" indica a R que devuelva la predicción de clase real.

# Paso 1. Número semilla
set.seed(1) # Para reproducibilidad

# Paso 2. Dividimos los datos
indice <- createDataPartition(base_modif$depression, p = 0.7, list = FALSE)

# Paso 3. Creamos conjunto de prueba y entrenamiento
entrenamiento_arbol <- base_modif[indice, ]
prueba_arbol <- base_modif[-indice, ]

# Paso 4. Ajustamos el modelo en el conjunto de entrenamiento
modelo_arbol <- rpart(depression ~ ., 
                      data = entrenamiento_arbol) 

#Visualizamos las reglas anidadas del modelo
print(modelo_arbol)

# Paso 5. Hacemos predicciones en los datos de prueba
pred_arbol <- predict(modelo_arbol, newdata = prueba_arbol , type = "class")

# Paso 6. Matriz de confusión y métricas de evaluación
matriz_confusión_arbol <- confusionMatrix(as.factor(pred_arbol), 
as.factor(prueba_arbol$depression), positive = "1")
print(matriz_confusión_arbol)


# Paso 7. Graficamos
rpart.plot(modelo_arbol)

#Estructura del árbol
#Nodo raíz (arriba)
#0.59: Proporción de casos con depresión en todo el dataset (59%)
#100%: Contiene el 100% de las observaciones

#Regla de división
#academic_pressure < 3
#El árbol divide los datos según si la presión académica es menor a 3
#Esta es la única variable que el algoritmo consideró importante

#Nodos terminales (hojas)
#Rama izquierda (presión académica < 3):
  
#0: Clasificación predicha = 0 (sin depresión)
#0.28: Solo 28% de este grupo tiene depresión
#32%: Representa el 32% del total de observaciones

#Rama derecha (presión académica ≥ 3):
  
#1: Clasificación predicha = 1 (con depresión)
#0.73: 73% de este grupo tiene depresión
#68%: Representa el 68% del total de observaciones

#Interpretación práctica
#El modelo es muy simple pero revelador:
  
#Baja presión académica (< 3): La mayoría NO tiene depresión (72% están bien)
#Alta presión académica (≥ 3): La mayoría SÍ tiene depresión (73% la padecen)

#¿Qué pasa si quiero modificar algunos hiperparametros?

#cp significa "complexity parameter" (parámetro de complejidad)
#Es un parámetro de regularización que controla el crecimiento del árbol
#Determina cuándo vale la pena hacer una nueva división en el árbol

#cp alto (ej: 0.1): Árbol simple, pocas divisiones
#cp bajo (ej: 0.005): Árbol complejo, muchas divisiones

control_params <- rpart.control(cp=0.005)  

# Ajustar el árbol de clasificación
modelo_arbol_ajustado <- rpart(depression ~ ., 
                               data = entrenamiento_arbol, 
                               control = control_params)

print(modelo_arbol_ajustado)

rpart.plot(modelo_arbol_ajustado)

# Predicciones con el modelo ajustado
pred_arbol_ajustado <- predict(modelo_arbol_ajustado, newdata = prueba_arbol, type = "class")

# Matriz de confusión para el modelo ajustado
matriz_confusion_ajustado <- confusionMatrix(as.factor(pred_arbol_ajustado), as.factor(prueba_arbol$depression), positive = "1")
print(matriz_confusion_ajustado)


# Validación cruzada ------------------------------------------------------

#A continuación, consideramos si podar el árbol podría conducir a mejores resultados. La función cv.tree() realiza una validación cruzada para determinar el nivel óptimo de complejidad del árbol; La poda de complejidad de costos se utiliza para seleccionar una secuencia de árboles para su consideración. Usamos el argumento FUN = prune.misclass para indicar que queremos que la tasa de error de clasificación guíe el proceso de poda y validación cruzada, en lugar del valor predeterminado para la función cv.tree(), que es la desviación. La función cv.tree() reporta el número de nodos terminales de cada árbol considerado (tamaño) así como la tasa de error correspondiente y el valor del parámetro costo-complejidad utilizado (k, que corresponde a α en (8.4)).


set.seed(7)
cv.depression <- cv.tree(tree.depression, FUN = prune.misclass)
names(cv.depression)

#A pesar de su nombre, dev corresponde al número de errores de validación cruzada.

par(mfrow = c(1, 2))
plot(cv.depression$size, cv.depression$dev, type = "b")
plot(cv.depression$k, cv.depression$dev, type = "b")

#Ahora aplicamos la función prune.misclass()
prune.depr <- prune.misclass(tree.depression, best = 2)
plot(prune.depr)
text(prune.depr, pretty = 0)


# Predicciones con el árbol podado
pred_podado <- predict(prune.depr, newdata = prueba_arbol, type = "class")

# Matriz de confusión para el árbol podado
matriz_confusion_podado <- confusionMatrix(as.factor(pred_podado), 
as.factor(prueba_arbol$depression), positive = "1")
print(matriz_confusion_podado)


# Comparación con modelo logístico ----------------------------------------

# Ajustamos el modelo de regresión logística en los datos de entrenamiento
modelo_log <- glm(depression  ~ ., 
                  data = entrenamiento_arbol, 
                  family = binomial(link="logit"))

# Ver resumen del modelo (opcional)
summary(modelo_log)


# Predicciones (probabilidades) en el conjunto de prueba
probabilidades_log <- predict(modelo_log, 
                              newdata = prueba_arbol, 
                              type = "response")

# Convertir probabilidades en clases predichas
predicciones_log <- ifelse(probabilidades_log >= 0.5, 1, 0)  

# Crear la matriz de confusión con métricas completas
matriz_confusion_log <- confusionMatrix(as.factor(predicciones_log), as.factor(prueba_arbol$depression), positive = "1")
matriz_confusion_log




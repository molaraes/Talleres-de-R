# =========================================================
# Knn vecinos
# Fecha: 16/06/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "caret", "forcats", "janitor", "MASS", "class", "reshape2")

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

# Sólo vamos a considerar:

# Dos variables numéricas
# Academic.Pressure y Study.Satisfaction

# Dos variable categóricas nominales
# Gender y City

# Dos varibles categóricas ordinales
# Sleep.Duration y Dietary.Habits

#Generamos una nueva base de datos
base_knn <- base %>% 
  dplyr::select(academic_pressure, study_satisfaction, gender, city, sleep_duration,dietary_habits)

# Preparación de variables  -------------------------------------------

## Vamos a transformar variables ordinales en numéricas  -------------------------------------------

#Variable Sleep.Duration:
base_knn$sleep_duration <- factor(base_knn$sleep_duration,levels = c("Less than 5 hours","5-6 hours", "7-8 hours", 
"More than 8 hours","Others"))

#La convierto a números de 1 al 5 con la función as.numeric()
base_knn$sleep_duration <- as.numeric(base_knn$sleep_duration)
table(base_knn$sleep_duration)

# Variable Dietary.Habits: 
base_knn$dietary_habits <- factor(base_knn$dietary_habits ,levels = c("Unhealthy","Moderate", "Healthy","Others"))
#La convierto a números de 1 al 5 con la función as.numeric()
base_knn$dietary_habits  <- as.numeric(base_knn$dietary_habits)
table(base_knn$dietary_habits)

## Vamos a escalar variables numéricas -------------------------------------------
escalamiento <- preProcess(base_knn[, c("academic_pressure", 
                                                   "study_satisfaction",
                                                   "sleep_duration",
                                                   "dietary_habits")], 
                           method = c("center", "scale"))

#Objeto tipo "preProcess" guarda información del escalamiento
class(escalamiento)

#Creamos base de datos con variables escaladas
base_knn_num_escaladas <- predict(escalamiento, base_knn[, c("academic_pressure","study_satisfaction","sleep_duration","dietary_habits")]) 
View(base_knn_num_escaladas)

#Podemos hacer una matriz de correlación
base_knn_num_escaladas %>% 
 cor()-> matriz_cor

#Redondeo a 3 decimales
matriz_cor <- round(matriz_cor,3)
matriz_cor

#Transformamos la matriz a formato largo
data_cor <- melt(matriz_cor)
View(data_cor)

#Crear el mapa de calor con valores dentro de las celdas
ggplot(data_cor, aes(x = Var1, y = Var2, fill = value)) +
  #Crear una cuadrícula para el mapa de calor
  geom_tile(color = "white") +
  #Crear un gradiente de dos colores scale_fill_gradient2()
  scale_fill_gradient2(low = "tomato2", high = "steelblue", mid = "white", midpoint = 0) +
  #Agrega texto a las celdas de la cuadrícula
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + # Agregar valores
  labs(title = "Mapa de calor de la matriz de correlación",
       x = "Variables",
       y = "Variables",
       fill = "Correlación") +
  theme_minimal()+
  #Rotar las etiquetas del eje x en 45 grados
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Vamos a generar variables dummy a partir de nominales -------------------------------------------

# Variable Gender:
# Se convierte en factor
table(base_knn$gender)
base_knn$gender <- factor(base_knn$gender, levels = c("Male", "Female"))

#Reagrupamos la variable City y al final eliminamos la que tiene muchos niveles
base_knn <- base_knn  %>%
  mutate(city_reagrupada = fct_lump_prop(city, prop = 0.04, other_level = "Otros")) %>%
  dplyr::select(-city)

#Convertimos a factor
table(base_knn$city_reagrupada)
base_knn$city_reagrupada <- factor(base_knn$city_reagrupada,levels = c("Otros","Hyderabad", "Kalyan", "Lucknow", "Srinagar", "Thane", "Vasai-Virar"))


#Creación de objeto tipo "dummyVars" 
dummy <- dummyVars("~ city_reagrupada + gender", data = base_knn)

#Objeto tipo "dummyVars" guarda información de las variables ficticias
class(dummy)

#Hacemos base de datos:
base_knn_nom_ficticias <- as.data.frame(predict(dummy, newdata = base_knn))
View(base_knn_nom_ficticias)

## Juntamos variable respuesta con predictores escalados y variables ficticias -------------------------------------------
base_knn_final <- cbind(Depression=base$depression,
                                   base_knn_num_escaladas, 
                                   base_knn_nom_ficticias)

View(base_knn_final)


# Datos de entrenamiento y prueba -----------------------------------------

#https://x.com/RosanaFerrero/status/1933766473664602477

# Paso 1. Número semilla
set.seed(123)

# Paso 2. Vector con los índices de los datos de entrenamiento:
indice <- createDataPartition(y=factor(base_knn_final$Depression), 
                              p = 0.7, list = FALSE)  # 70% entrenamiento



# Paso 3. Dividir los datos
entrenamiento_knn <- base_knn_final[indice, ]
prueba_knn <- base_knn_final[-indice, ]

#Paso 4. Revisar dimensiones de los dos marcos de datos
dim(entrenamiento_knn)
dim(prueba_knn)


# Modelo de vecinos más cercanos ------------------------------------------

#Con 5 vecinos
k <- 5  # Número de vecinos

#En train y test van las bases de datos sin la variable respuesta
entrenamiento_knn_sin_respuesta <- entrenamiento_knn %>% dplyr::select(-Depression)
prueba_knn_sin_respuesta <- prueba_knn %>% dplyr::select(-Depression)

#Se ajusta el modelo de KNN
knn_pred <- knn(train = entrenamiento_knn_sin_respuesta, 
                test = prueba_knn_sin_respuesta, 
                cl = entrenamiento_knn$Depression, 
                k = k)


# Evaluación del modelo
matriz_confusión_knn <- confusionMatrix(as.factor(knn_pred), as.factor(prueba_knn$Depression), positive = "1")
print(matriz_confusión_knn)


# Probamos con diferentes niveles de K ------------------------------------

k_values <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21)

accuracy_results <- data.frame(k = k_values, accuracy = NA, 
                               sensitivity = NA, specificity = NA)

entrenamiento_sin_respuesta <- entrenamiento_knn %>% dplyr::select(-Depression)
prueba_sin_respuesta <- prueba_knn %>% dplyr::select(-Depression)

for(i in 1:length(k_values)) {
  k_val <- k_values[i]
  knn_pred_temp <- knn(train = entrenamiento_sin_respuesta,  
                       # Datos de entrenamiento (sin variable respuesta)
                       test = prueba_sin_respuesta,          
                       # Datos de prueba (sin variable respuesta)  
                       cl = entrenamiento_knn$Depression,    
                       # Clases verdaderas del entrenamiento (0 o 1)
                       k = k_val)                            
  # Número de vecinos a usar (1, 3, 5, etc.)
  cm_temp <- confusionMatrix(as.factor(knn_pred_temp),      
                             # Predicciones del modelo (convertidas a factor)
                             as.factor(prueba_knn$Depression), 
                             # Valores reales de prueba (convertidas a factor)
                             positive = "1")                  
  # Define que "1" es la clase positiva (depresión)
  accuracy_results$accuracy[i] <- cm_temp$overall['Accuracy']
  accuracy_results$sensitivity[i] <- cm_temp$byClass['Sensitivity']
  accuracy_results$specificity[i] <- cm_temp$byClass['Specificity']
}

# Gráfico de optimización de k
accuracy_results %>%
  pivot_longer(c(accuracy, sensitivity, specificity), 
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = k, y = value, color = metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Optimización del Valor K",
       x = "Número de Vecinos (k)",
       y = "Valor de la Métrica",
       color = "Métrica") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylim(0.5, 1)

# Mejor k
mejor_k <- k_values[which.max(accuracy_results$accuracy)]
mejor_k

# Modelo con mejor k
knn_pred_final <- knn(train = entrenamiento_sin_respuesta,
                      test = prueba_sin_respuesta,
                      cl = entrenamiento_knn$Depression,
                      k = mejor_k)

# Matriz de confusión final
matriz_confusion_final <- confusionMatrix(as.factor(knn_pred_final), 
                                          as.factor(prueba_knn$Depression), 
                                          positive = "1")
print(matriz_confusion_final)

# Matriz de confusión como heatmap
cm_table <- as.table(matriz_confusion_final$table)
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicción", "Realidad", "Frecuencia")

ggplot(cm_df, aes(x = Predicción, y = Realidad, fill = Frecuencia)) +
  geom_tile() +
  geom_text(aes(label = Frecuencia), color = "white", size = 6) +
  theme_minimal() +
  labs(title = paste("Matriz de Confusión - KNN (k =", mejor_k, ")"),
       x = "Predicción", y = "Valor Real") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")


# Curva de ROC ------------------------------------------------------------

# Modelo final CON probabilidades
knn_pred_final_prob <- knn(train = entrenamiento_sin_respuesta,
                           test = prueba_sin_respuesta,
                           cl = entrenamiento_knn$Depression,
                           k = mejor_k,
                           prob = TRUE)  

# Extraemos las probabilidades
probabilidades <- attr(knn_pred_final_prob, "prob")

# Ajustamos direccionalidad (k-NN da prob de la clase predicha)
# Si predijo "0", la prob de "1" es (1 - prob)
prob_clase_1 <- ifelse(knn_pred_final_prob == "1", 
                       probabilidades, 
                       1 - probabilidades)


# Creamos curva ROC
roc_curve <- roc(prueba_knn$Depression, prob_clase_1)

# Graficamos curva ROC
plot(roc_curve, 
     main = paste("Curva ROC - k-NN (k =", mejor_k, ")"),
     col = "blue", 
     lwd = 2)

# Añadimos línea de referencia (modelo aleatorio)
abline(a = 0, b = 1, lty = 2, col = "red")

# Mostramos AUC
auc_value <- auc(roc_curve)
legend("bottomright", 
       legend = paste("AUC =", round(auc_value, 3)),
       col = "blue", 
       lwd = 2)

print(paste("AUC:", round(auc_value, 3)))


# Práctica ----------------------------------------------------------------

# 1. Estima una regresión logística utilizando las mismas variables predictoras.
# 2. Divide tus datos de tu regresión logística en entrenamiento y prueba.
# 3. Elabora la matriz de confusión.
# 4. Compara los resultados de la regresión logística con el knn que hicimos. 
# 5. Replica el ejercicio con un mayor número de kvecinos.


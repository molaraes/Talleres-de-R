# =========================================================
# Clústers- jerárquico
# Fecha: 14/07/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------
pacman::p_load("tidyverse", "factoextra", "cluster", "patchwork")

# Directorio
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MGAP/Datos")

#Base
base <- read.csv("TENBIARE.csv")
names(base)

#Variables:
#PA3_01: qué tan satisfecho con su nse
#PA3_02: qué tan satisfecho con su salud
#PA3_03: qué tan satisfecho con su logros
#PA3_04: qué tan satisfecho con su relaciones
#PA3_05: qué tan satisfecho con su vida social
#PA3_06: qué tan satisfecho con su vida familiar
#PA3_07: qué tan satisfecho con su vida afectiva
#PA3_08: qué tan satisfecho con su perspectivas a futuro
#PA3_09: qué tan satisfecho con su tiempo libre
#PA3_10: qué tan satisfecho con su libertad
#PA3_11: qué tan satisfecho con su seguridad
#PA3_12: qué tan satisfecho con su actividad principal
#PA3_13: qué tan satisfecho con su vivienda
#PA3_14: qué tan satisfecho con su vecindario
#PA3_15: qué tan satisfecho con su ciudad
#PA3_16: qué tan satisfecho con su servicios públicos
#PA3_17: qué tan satisfecho con su pais


# Preparación de los datos ------------------------------------------------

base_modif1 <- base %>% 
  select(ENT, starts_with("PA3_"), FAC_ELE) %>% #selecciono variables
  na.omit() %>%  #omitir datos perdidos
  mutate_all(as.numeric) %>% #convertir a numéricas
  group_by(ENT) %>% #agrupa por entidad
  summarise(across(everything(), mean)) #se calcula promedio

## Ponderamos ------------------------------------------------
base_modif2 <- base_modif1 %>% 
  group_by(ENT) %>%  #agrupo por entidad
  summarise(across(starts_with('PA3_'), #selecciono variables
                   ~ weighted.mean(., FAC_ELE, na.rm=T))) #calculo media ponderada de las 17 variables por entidad


## Estandarizamos ------------------------------------------------
#Habiendo ya seleccionado las variables, el primer paso es estandarizarlas. 
base_num <- scale(base_modif2[,2:18]) #selecciono columnas 2 a 18
rownames(base_num) <- base_modif2$ENT #pongo como ids de filas las ent
base_num <- as_tibble(base_num) #guardo como base de datos


#### Método jerárquico aglomerativo ----
#Dada una tabla de datos numéricos, un método de clustering jerárquico aglomerativo usa una matriz D de distancias entre las filas de la tabla de datos para ir agrupando secuencialmente los objetos que representan estas filas. 

#La matriz de distancias D entre los objetos se puede calcular con 
#la función dist, cuya sintaxis básica es

#dist(x, method=...)

#donde

#x es nuestra tabla de datos (una matriz o un data frame de variables cuantitativas).

#method sirve para indicar la distancia que queremos usar, cuyo nombre se ha de entrar entrecomillado. La distancia por defecto es la euclidiana.

#Calculamos la matriz de distancias
mat_dist <- dist(x=base_num, method="euclidean")

#Una vez calculada la matriz de distancias, los diferentes métodos de clustering jerárquico aglomerativos están implementados en R en la función hclust, cuya sintaxis básica es la siguiente:

#hclust(d, method=...)

#d es la matriz de distancias entre nuestros objetos 

#method sirve para especificar cómo se define la distancia de la unión de dos clusters al resto de los clusters 


#La función hclust tiene muchos métodos 

# MÉTODO 1: Complete Linkage
# - Une clusters por la MAYOR distancia entre puntos de diferentes clusters
clus_complete <- hclust(d = mat_dist, method = "complete")

# MÉTODO 2: Average Linkage  
# - Une clusters por la distancia PROMEDIO entre todos los puntos
clus_average <- hclust(d = mat_dist, method = "average")

# MÉTODO 3: Ward D2
# - Une clusters MINIMIZANDO la varianza total (suma de cuadrados)
clus_ward <- hclust(d = mat_dist, method = "ward.D2")


#Podemos ver las correlaciones

#La correlación cofonética mide qué tan bien el dendrograma preserva las distancias originales de los datos.

cor_complete <- cor(mat_dist, cophenetic(clus_complete))
cor_average <- cor(mat_dist, cophenetic(clus_average))
cor_ward <- cor(mat_dist, cophenetic(clus_ward))


##### Graficamos -----

#Para poder comprender un clustering jerárquico, lo mejor es dibujarlo en forma de dendrograma, con los objetos que clasificamos en las hojas. 
#Para ello le aplicamos simplemente la función plot. 

#Hay dos parámetros que hay que tener en cuenta (aparte de los usuales):

#hang, que controla la situación de las hojas del dendrograma respecto del margen inferior.
#cex, controla el tamaño de la letra


#Dendograma
#hang: es para los valores de las hojitas
#cex: para el tamaño letra


par(mfrow = c(2, 2))  # Organizar gráficos en 2x2

# Dendrograma Complete Linkage
plot(clus_complete, 
     main = paste("Complete Linkage (r =", round(cor_complete, 3), ")"),
     cex = 0.6,           # Tamaño de texto
     hang = -1,           # Alinear hojas al margen inferior
     xlab = "Estados",
     ylab = "Distancia")

# Dendrograma Average Linkage  
plot(clus_average, 
     main = paste("Average Linkage (r =", round(cor_average, 3), ")"),
     cex = 0.6, 
     hang = -1,
     xlab = "Estados", 
     ylab = "Distancia")

# Dendrograma Ward D2
plot(clus_ward, 
     main = paste("Ward D2 (r =", round(cor_ward, 3), ")"),
     cex = 0.6, 
     hang = -1,
     xlab = "Estados",
     ylab = "Distancia")

par(mfrow = c(1, 1))  # Restaurar configuración de gráficos

str(clus_average)

#Como vemos, un clustering jerárquico producido con hclust es una lista. 
#Sus dos componentes más interesantes para nosotros son:

#Por un lado, merge es una matriz de dos columnas que indica el orden en el que se han ido agrupando los objetos de dos en dos. En esta matriz, los objetos originales se representan con números negativos, y los nuevos clusters con números positivos que indican el paso en el que se han creado. 

#merge
clus_average$merge

#La otra componente que nos interesa es height, un vector que contiene las distancias a las que se han ido agrupando los pares de clusters, representadas como alturas en el eje de ordenadas en el dendrograma. 

#height
clus_average$height
clus_complete$height
clus_ward$height

#Dónde cortar el dendograma
#Con R disponemos de dos funciones básicas para obtener agrupamientos a partir de un clustering jerárquico. La primera es la función cutree. 

#Su sintaxis básica es

#cutree(hclust, k=..., h=...)

#donde hclust es el resultado de una función, y se ha de especificar o bien el parámetro k que indica el número de clusters deseado o bien el parámetro h que indica la altura a la que queremos cortar.

cutree(clus_complete, h=11) #es la altura
cutree(clus_complete, k=2) #k es el num clusters

cutree(clus_average, h=11) #es la altura
cutree(clus_average, k=2) #k es el num clusters

cutree(clus_ward, h=11) #es la altura
cutree(clus_ward, k=2) #k es el num clusters

#Otra posibilidad es usar la función rect.hclust, que sobre el dendrograma dibujado en la instrucción inmediatamente anterior resalta los grupos enmarcándolos en rectángulos. La sintaxis es similar:

#se aplica al resultado de un hclust y al número k de grupos o a la altura h. Admite además un parámetro border que permite especificar los colores de los rectángulos (por defecto, todos rojos).

#visualizar
plot(clus_complete, hang=-1, cex=0.6)
rect.hclust(clus_complete, h=11)

plot(clus_complete, hang=-1, cex=0.6)
rect.hclust(clus_complete, k=4)

plot(clus_average, hang=-1, cex=0.6)
rect.hclust(clus_average, h=11)

plot(clus_average, hang=-1, cex=0.6)
rect.hclust(clus_average, k=4)

plot(clus_ward, hang=-1, cex=0.6)
rect.hclust(clus_ward, h=11)

plot(clus_ward, hang=-1, cex=0.6)
rect.hclust(clus_ward, k=4)

#Dendograma con factoextra
p1 <- fviz_dend(clus_average, 
                cex = 0.6,                    # Tamaño de texto
                k = 3,                        # Número de clusters
                rect = TRUE,                  # Mostrar rectángulos
                rect_fill = TRUE,             # Rellenar rectángulos
                rect_border = "black",        # Color del borde
                labels_track_height = 0.8,    # Altura de las etiquetas
                main = "Dendrograma Complete Linkage (k=3)")

print(p1)

p1 | p2 | p3

# Comparar los tres métodos con k=3
p2 <- fviz_dend(clus_complete, cex = 0.6, k = 3, rect = TRUE, 
                main = "Average Linkage (k=3)")
p3 <- fviz_dend(clus_ward, cex = 0.6, k = 3, rect = TRUE,
                main = "Ward D2 (k=3)")

# ANÁLISIS DESCRIPTIVO DE CLUSTERS ---------------------------------------

# Unir datos originales con asignación de clusters
grupos_finales <- cutree(clus_complete, k = 3)
datos_con_clusters <- cbind(base_modif2, cluster = grupos_finales)

medias_por_cluster <- datos_con_clusters %>%
  group_by(cluster) %>%
  summarise(across(starts_with("PA3_"), mean), .groups = 'drop')

datos_con_clusters %>% 
  filter(cluster==1) %>% 
  select(ENT)

datos_con_clusters %>% 
  filter(cluster==2) %>% 
  select(ENT)

datos_con_clusters %>% 
  filter(cluster==3) %>% 
  select(ENT)

# VALIDACIÓN Y DIAGNÓSTICO -----------------------------------------------
silhouette_scores <- silhouette(grupos_finales, mat_dist)
silhouette_scores

sil_promedio <- mean(silhouette_scores[, "sil_width"])
sil_promedio

# Gráfico de silueta
fviz_silhouette(silhouette_scores, 
                palette = c("#E74C3C", "#3498DB", "#2ECC71"),
                ggtheme = theme_minimal()) +
  labs(title = "Análisis de Silueta por Cluster",
       subtitle = paste("Coeficiente promedio:", round(sil_promedio, 3)))


# =========================================================
# Clústers -- Kmedias
# Fecha: 01/07/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------
pacman::p_load("tidyverse", "factoextra", "cluster")

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


# Método k medias ---------------------------------------------------------


#Gráfico del codo
fviz_nbclust(base_num, kmeans, 
             method="wss")

#Gráfico de la silueta
fviz_nbclust(base_num, kmeans, 
             method="silhouette")

#kmeans(x, centers)
#x, es la base de datos o matriz
#centers, el número de clusters

#con 2 clusters
set.seed(100)
kmeansfit2 <- kmeans(base_num, centers=2)
kmeansfit2
#con 3 clusters
set.seed(100)
kmeansfit3 <- kmeans(base_num, centers=3)
kmeansfit3
#con 4 clusters
set.seed(100)
kmeansfit4 <- kmeans(base_num, centers=4)
kmeansfit4


# Gráfico -----------------------------------------------------------------

#extraigo elemento cluster de la lista "kmeansfit"

cluster <- kmeansfit2$cluster

clusplot(base_num, cluster, shade=T,
         lines=0, labels=3, color=T,
         col.p="black", cex.txt=0.75,
         main="Análisis de Clusters", sub="")

#Con facto extra
fviz_cluster(kmeansfit2, base_num,
             star.plot=T, show.clust.cent = T,
             ellipse = T)+
  labs(title="Resultados clusters")+
  theme_minimal()



# Análisis posterior ------------------------------------------------------

# Agregamos los clusters de la solución de 2 clusters
base_modif2$cluster <- kmeansfit2$cluster

# Vemos cuántas entidades hay en cada cluster
table(base_modif2$cluster)

# Vemos algunas entidades de cada cluster
base_modif2 %>% 
  select(ENT, cluster) %>% 
  arrange(cluster)

# Estadísticas por cluster
base_modif2 %>% 
  group_by(cluster) %>% 
  summarise(
    n_entidades = n(),
    across(starts_with("PA3"), ~ mean(.x, na.rm = T), .names = "promedio_{.col}")
  ) -> tabla



# Ejercicio ---------------------------------------------------------------

# 1. Replicar el ejercicio de clústers con estos datos: https://www.kaggle.com/datasets/jainaru/world-happiness-report-2024-yearly-updated
# 2. ¿Cómo se conforman?, ¿qué tienen en común los países en cada clúster?

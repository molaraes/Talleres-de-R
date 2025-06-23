# =========================================================
# Componentes principales
# Fecha: 24/06/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------
pacman::p_load("tidyverse", "factoextra", "GGally", "ggcorrplot", "FactoMineR")

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
  dplyr::select(ENT, starts_with("PA3_"), FAC_ELE) %>% #selecciono variables
  na.omit() %>%  #omitir datos perdidos
  mutate_all(as.numeric) %>% #convertir a numéricas
  group_by(ENT) %>% #agrupa por entidad
  summarise(across(everything(), mean)) #se calcula promedio

## Ponderamos -----
base_modif2 <- base_modif1 %>% 
  group_by(ENT) %>%  #agrupo por entidad
  summarise(across(starts_with('PA3_'), #selecciono variables
            ~ weighted.mean(., FAC_ELE, na.rm=T))) #calculo media ponderada de las 17 variables por entidad


# Estandarizamos ----------------------------------------------------------

#Habiendo ya seleccionado las variables, el primer paso es estandarizarlas. 

base_num <- scale(base_modif2[,2:18]) #selecciono columnas 2 a 18

rownames(base_num) <- base_modif2$ENT #pongo como ids de filas las ent

base_num <- as_tibble(base_num) #guardo como base de datos

## Examinamos la correlacion ----

#El siguiente paso es observar la correlación entre variables. Esto es útil para saber cómo se relacionan las variables elegidas, y también para ver si hay correlaciones extremadamente altas entre dos o más variables. Si se da el caso de que tenemos dos o más variables con alta correlación entre ellas, éstas tendrán una enorme influencia en la orientación de los componentes.

#Matriz de correlación
cor(base_modif2[,2:18], method = "pearson", use = "complete.obs")

#Matriz de correlación con datos estandarizados (es lo mismo)
correlacion <- base_num %>% 
  cor(use = "pairwise") %>% 
  round(3)

#Gráfico 1
ggcorrplot(correlacion, type = "lower", lab = T, show.legend = F)

#Gráfico 2
ggpairs(base_modif2[,2:18],axisLabels="show")+
  theme_void()+
  theme(strip.text.x = element_text(size=5,angle=90))


# Componentes principales -------------------------------------------------

#Una vez que vemos las correlaciones, estamos listes para hacer el PCA. Para ello, utilizaremos dos paquetes diferentes que realizan el PCA

#El PCA nos permitirá definir a través de los componentes cuáles serán las dimensiones latentes, para luego avanzar a realizar un índice 

#Primero, con el subconjunto de datos, generamos un PCA con el paquete de stats, que es uno de los paquetes básicos de R (ya viene instalado). Luego con el comando summary() podemos ver lo que el análisis nos da.

pca <- princomp(base_num)
summary(pca, loadings = T)

# Desviaciones estándar de cada componente
# Proporción de varianza 
# Varianza acumulada
# Los loadings (cargas o pesos) muestran cómo cada variable contribuye a cada componente: valores altos (positivos o negativos nos dice qué tanta influencia tiene la variable en cada componente)

#También podemos ver cómo se construye cada componente con las variables que seleccionamos.

#El eigen valor indica cuánto se estira o se contrae un vector propio cuando una matriz actúa sobre él (bajo una transformación lineal)
#Si tienes una matriz A y un vector v, y al aplicar la transformación de A a v, el resultado es 2v (es decir, el vector v se duplica en longitud), entonces 2 es el eigenvalor correspondiente al eigenvector v. El eigenvector es un vector que después de ser transformado, sólo cambia de magnitud, no de dirección.

eig_val <- get_eigenvalue(pca)
eig_val

#Te dicen qué tan importante es cada componente
#Cuánta varianza explica cada uno
#Son como el "tamaño" o "fuerza" de cada componente

#Eigenvalue = "Esta torta pesa 2 kilos" (importancia)
#Loading = "La receta: 3 huevos + 2 tazas harina + 1 taza azúcar" (composición)


#Otra forma de analizar esta información es a través de gráficos. El siguiente comando nos da en su forma más simple, el porcentaje de varianza explicada por cada uno de los componentes.

fviz_eig(pca, addlabels = T, ylim = c(0, 50))

#Este es un Scree Plot (gráfico de sedimentación), ayuda a decidir cuántos componentes principales conservar.
#Buscar donde la línea hace un "codo" o se vuelve más plana.
#Despúes del componente tres, la caída es muy gradual.

#Un pequeño cambio en el comando, también nos da un gráfico con los valores propios de cada uno de los componentes:
fviz_eig(pca, choice = c("eigenvalue"), addlabels = T, ylim = c(0, 12))

#Para saber cómo está compuesto cada uno de estos componentes, podemos generar un Biplot. Este tipo de gráfico nos aparecerá como vectores en dos dimensiones (que serán los dos primeros componentes del análisis).

fviz_pca_biplot(pca, repel = F, col.var = "black", col.ind = "gray")

#Componente 1
#Todas las variables contribuyen positivamente (todas las flechas van hacia la derecha)
#Es un factor general de satisfacción con la vida

#Componente 2: personal vs contextual
#Arriba: Satisfacción personal/individual
#PA3_02 (salud), PA3_03 (logros), PA3_10 (libertad)
#Abajo: Satisfacción contextual/social
#PA3_11 (seguridad), PA3_15 (ciudad), PA3_17 (país)

#Muy correlacionadas (flechas cercanas):
#PA3_02, PA3_03, PA3_10: Salud, logros, libertad (bienestar personal)
#PA3_15, PA3_17: Ciudad y país (contexto macro)
#PA3_05, PA3_09: Vida social y tiempo libre (ocio social)

#PA3_11 (seguridad) es especial

#Para saber qué tanto contribuyen las variables dentro de cada dimensión
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)

#A medida que se crean estas nuevas dimensiones, pueden considerarse como nuevas variables a integrar en nuestra base de datos original, como se muestra a continuación:
base_modif2 <- bind_cols(base_modif2, as_tibble(pca$scores))


# Otra opción para PCA ----------------------------------------------------


#Para esta parte, generaremos de nuevo un PCA, pero esta vez lo haremos con el comando PCA del paquete  FactoMineR. Esto nos dará el mismo resultado que el  paquete anterior, pero queremos mostrar que esta herramienta puede ser utilizada de varias maneras en R.

pca_1 <- PCA(base_num, graph = F)
pca_1
summary(pca_1)

#var$coord: coordenadas de variables para crear un diagrama de dispersión
#var$cos2: representa la calidad de representación de las variables en el mapa de factores, y se calcula como (var$coord)^2
#var$contrib: contiene las contribuciones (en porcentaje) de las variables a los componentes principales, y es calculada con (var.cos2 * 100) / (cos2 total del componente).

#Como vimos antes, podríamos retener los componentes que contienen un Eigenvalor mayor que 1, que podemos ver de nuevo en el gráfico:

fviz_eig(pca_1, choice = "eigenvalue", addlabels = T, ylim = c(0, 3))

#También podemos hacer un biplot con la contribución de cada variable
fviz_pca_biplot(pca_1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#Como pudimos ver de antemano, los componentes mayores de 1 son los tres primeros.  Para condensar los componentes elegidos en una sola variable, es necesario recordar cuánta varianza acumulada representan del total:

eig <- get_eig(pca_1)
eig

#Podemos crear un índice ahora con los componentes
#Veamos cada entidad

pca_1$ind$coord
#Eso es la matriz de coordenadas (scores) de tus individuos en los componentes principales.

data_pca <- pca_1$ind$coord %>% # Obtiene la matriz de coordenadas (scores) de cada entidad en los componentes principales
  as_tibble() %>%
  # Convierte la matriz a formato tibble (más fácil de manipular con dplyr)
  mutate(pca_01 = (Dim.1 * 11.169 + Dim.2 * 1.934 + Dim.3 * 0.987) / (11.169 + 1.934 + 0.987))

base_modif2 <- bind_cols(base_modif2, data_pca %>% dplyr::select(pca_01))

#Reescalamos

base_modif2 <- base_modif2 %>%
  mutate(ind_satis = GGally::rescale01(pca_01) * 100)%>%
  dplyr::select(ind_satis, everything())

#Ya con nuestro nuevo índice reescalado, podemos ver cómo se ve su densidad:

index_density <- ggplot(data = base_modif2, mapping = aes(x = ind_satis)) + 
  labs(x=" Índice de Satisfacción", y = "densidad") +
  geom_density()
index_density


# Se normaliza el Índice
base_modif2$indice_norm <- (base_modif2$pca_01 - mean(base_modif2$pca_01)) / sd(base_modif2$pca_01)

base_modif2 %>% 
  dplyr::select(ENT, indice_norm, ind_satis)  


#Referencias
#https://arcruz0.github.io/libroadp/indexes.html
#https://mr-hn.github.io/pcaIndex/
#https://bookdown.org/jsalinas/tecnicas_multivariadas/acp.html
#También pueden ver esta práctica
#https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/R/code/Chapter%207%20-%20Unsupervised%20Learning.R
#Y este libro
#https://www.oreilly.com/library/view/practical-statistics-for/9781492072935/
#Más referencias
#https://rpubs.com/Cristina_Gil/PCA
#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
#https://cienciadedatos.net/documentos/35_principal_component_analysis
#https://rpubs.com/AlithC/Entrega4_Cap3-PCA
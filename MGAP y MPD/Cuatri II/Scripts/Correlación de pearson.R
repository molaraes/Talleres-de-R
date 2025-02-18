# =========================================================
# Análisis de correlación
# Fecha: 10/02/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------
#Paquetes
pacman::p_load("readxl", "tidyverse", "GGally")

#Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Lectura de datos
base_marginacion <- read_excel("IMM_2020.xlsx", 
                               sheet = "IMM_2020")

#Vamos a analizar la relación entre el porcentaje de personas en viviendas sin agua entubada y con piso de tierra.

#Antes de hacer un análisis de correlación es importante hacer un gráfico de dispersión

base_marginacion %>%                                        ggplot(aes(x=OVSAE,y=OVPT))+    
  geom_point(color="midnightblue")+             
  geom_smooth(method="lm")+
  labs(
    title="Relación entre porcentaje de viviendas con piso de tierra y sin agua entubada, a nivel municipal 2020",
    x= "Porcentaje de personas en viviendas de agua entubada",
    y="Porcentaje de personas en viviendas con piso de tierra",
    caption="Fuente: elaboración propia con datos de CONAPO"
  )+
  theme_light() 

#¿Qué relación se observa entre las variables?

#Vamos a seleccionar nuestras dos variables y calculamos el índice de correlación
subset1 <- base_marginacion %>%
  select(OVSAE, OVPT) 

#Hacemos la matriz de correlación
corMatrix1 <- round(cor(subset1, method = "pearson", use= "pairwise.complete.obs"), 2)
corMatrix1

#Pero, ¿cómo sabemos si la correlación es estadísticamente significativa?
cor.test(subset1$OVSAE, subset1$OVPT, 
         method = "pearson")

#Para cambiar a spearman podemos poner method="spearman"


#Podemos hacer un análisis de correlación de todas las variables
base_marginacion %>%
  select(-CVE_ENT, -NOM_ENT, -CVE_MUN, -NOM_MUN, -IMN_2020,-GM_2020) %>% 
  ggpairs()




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

#Vamos a analizar la relación entre el porcentaje de personas en viviendas sin agua entubada y el porcentaje de viviendas con piso de tierra.

#Antes de hacer un análisis de correlación es importante hacer un gráfico de dispersión

base_marginacion %>%                                        ggplot(aes(x=OVSAE,y=OVPT))+    
  geom_point(color="midnightblue")+           
  geom_smooth(method="lm")+
  labs(
    title="Relación entre porcentaje de viviendas con piso de tierra y\nsin agua entubada, a nivel municipal 2020",
    x= "Porcentaje de personas en viviendas de agua entubada",
    y="Porcentaje de personas en viviendas con piso de tierra",
    caption="Fuente: elaboración propia con datos de CONAPO"
  )+
  theme_light() +
  theme(
    plot.title=element_text(hjust=0.5)
  )

#¿Qué relación se observa entre las variables?

#Vamos a seleccionar nuestras dos variables y calculamos el índice de correlación
subset1 <- base_marginacion %>%
  select(OVSAE, OVPT) 

#Hacemos la matriz de correlación
corMatrix1 <- round(cor(subset1, method = "pearson", use= "pairwise.complete.obs"), 2)
corMatrix1

#round, redondea a dos decimales
#cor es la función para la correlación
#subset1, los datos que va a tomar
#method, calcula la correlación de pearson
#pairwise complete: utiliza pares de datos que estén sin NAs.

#Pero, ¿cómo sabemos si la correlación es estadísticamente significativa?
cor.test(subset1$OVSAE, subset1$OVPT, 
         method = "pearson")

#El coeficiente de correlación (cor) es 0.391, lo que indica:Una relación positiva moderada-débil: Cuando una variable aumenta, la otra tiende a aumentar también

#La significancia estadística es muy alta (p-value < 2.2e-16):Extremadamente inferior al nivel de 0.05

#El intervalo de confianza del 95% va de 0.357 a 0.424: Podemos estar 95% seguros de que la verdadera correlación en la población está en este rango

#El intervalo es relativamente estrecho, lo que sugiere una estimación precisa

#La prueba se realizó con 2,467 grados de libertad (df), indicando una muestra grande de 2,469 observaciones

#La hipótesis alternativa probada fue que la correlación es diferente de cero (two-tailed test)

#Para cambiar a spearman podemos poner method="spearman"

cor.test(subset1$OVSAE, subset1$OVPT, 
         method = "spearman")

#La correlación de Spearman (rho) es una medida estadística que evalúa la relación monótona entre dos variables, a diferencia de la correlación de Pearson que mide relaciones lineales.
#1) Se basa en rangos: Convierte los valores originales a rankings antes de calcular la correlación.
#2)Es no paramétrica: No asume una distribución normal de los datos (sirve en casos cuando nuestros datos no siguen una distribución normal)


#Podemos hacer un análisis de correlación de todas las variables
base_marginacion %>%
  select(-CVE_ENT, -NOM_ENT, -CVE_MUN, -NOM_MUN, -IMN_2020,-GM_2020) %>% #quitamos variables de identificación
  ggpairs()

#Ejercicio.
#Escoge otras dos variables, replica el diagrama de dispersión, el cálculo de la correlación y la prueba de significancia para el coeficiente de correlación.



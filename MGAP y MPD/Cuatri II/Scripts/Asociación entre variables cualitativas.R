# =========================================================
# Asociación entre variables cualitativas
# Fecha: 24/02/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------
#Paquetes
pacman::p_load("tidyverse", "janitor", "DescTools")

#Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Lectura de datos
base <- read_csv("TENBIARE.csv")

# Chi-cuadrada ------------------------------------------------------------

#Notación: X ~ χ²(k), donde k representa los grados de libertad.

#Dominio: Solo toma valores positivos (x ≥ 0).

#Forma: Es asimétrica, sesgada a la derecha. A medida que aumentan los grados de libertad, se vuelve más simétrica y se aproxima a una distribución normal.

#Nos vamos a preguntar si las variables son estadísticamente independientes o no.

#Se dice que las variables son independientes si las distribuciones condicionales de la población son idénticas. (Distribuciones condicionales vs distribuciones marginales)

#Dado que nosotres no trabajamos con los datos de la población y normalmente trabajamos con una muestra, testeamos la independencia en los datos de la muestra.

#Vamos a relacionar el hecho de pertencer a una organización o movimiento (PG1_1) al tamaño de localidad (TLOC)

base <- base %>% 
  mutate(PG1_1=factor(PG1_1, levels=c(1,2), 
                      labels=c("Sí", "No"))) %>% 
  mutate(urbano= ifelse(TLOC<4, 1, 0)) %>% 
  mutate(urbano=factor(urbano, levels=c(0,1),
                       labels=c("Rural", "Urbano")))

#Fórmula de chi cuadrada: https://simulacionutp2016.wordpress.com/2016/09/17/prueba-%CF%87%C2%B2-chi-cuadrado/ y grados de libertad: https://slideplayer.es/slide/1373569/


#Paso 1. Hacemos la tabla de contigencia para calcular las frecuencias observadas.
table(base$urbano, base$PG1_1)

#Paso 2. Obtenemos probabilidad marginal de la columna (probabilidad de participar)
base %>% 
  tabyl(urbano,PG1_1) %>% 
  adorn_totals(c("row", "col"))

prob_particip <- 1065/31166
prob_noparticip <- 30101/31166

#Paso 3. Obtenemos frecuencias esperadas (probabilidad de participar, sin importar el tamaño de la localidad)
prob_ru_part <- 7353*prob_particip
prob_ru_nopart <-7353*prob_noparticip
prob_urb_part <-23813*prob_particip
prob_urb_nopart <- 23813*prob_noparticip


#Paso 4. Aplico fórmula
chi_cuadrado <- ((((329-prob_ru_part)^2)/prob_ru_part)+
                   (((7024-prob_ru_nopart)^2)/prob_ru_nopart)+
                   (((736-prob_urb_part)^2)/prob_urb_part)+
                   (((23077-prob_urb_nopart)^2)/prob_urb_nopart))

chi_cuadrado

gl <- (2-1)*(2-1)
gl

# Para obtener el valor de tablase de chi-cuadrada con nivel de significancia 0.05 y 1 grados de libertad:
valor_chi <- qchisq(1 - 0.05, df = gl)
print(valor_chi)  # Aproximadamente 3.841459

#Nuestra chi-cuadrada empírica, es mayor a la chi-cuadrada teórica, por ello rechazo la hipótesis nula. Esto quiere decir que existe evidencia suficiente para afirmar una relación o asociación entre pertenecer a una zona rural o urbana y participar en algún movimiento u organización política o social.

options(scipen=999)

chisq.test(base$urbano, base$PG1_1, correct=F)


# Coeficiente de gamma ----------------------------------------------------

#El coeficiente gamma (γ) es una medida de asociación utilizada para analizar la relación entre dos variables ordinales. Su propósito principal es evaluar la fuerza y dirección de la relación entre estas variables, especialmente en tablas de contingencia.

#Se basa en la diferencia entre los pares concordantes y pares discordantes dentro de los datos.

#Par concordante: Dos observaciones (x₁,y₁) y (x₂,y₂) son concordantes si ambas variables se mueven en la misma dirección. Es decir, si x₂ > x₁ y y₂ > y₁, o si x₂ < x₁ y y₂ < y₁.

#Par discordante: Dos observaciones son discordantes si las variables se mueven en direcciones opuestas. Es decir, si x₂ > x₁ y y₂ < y₁, o si x₂ < x₁ y y₂ > y₁.

#Supongamos que tenemos dos observaciones x, y. Los valores de x son: 2, 3, 5, 1, 4. Y los de Y son: 1, 4, 2, 3, 5. El primer par (obs 1 con obs 2 es concordante).

#Su fórmula es C-D/C+D

#Haremos una relación entre haber tenido menos, similar o  más oportunidades de educación que sus padres y tener actualmente una menor, mediana o mayor satisfación con su vida.

base <- base %>% 
  mutate(PA1=as.numeric(PA1),
         PI3=as.numeric(PI3)) %>% 
  mutate(satis=
           case_when(
             PA1==0 & PA1<=3 ~ "Baja", 
             PA1>3 & PA1<=6 ~ "Media",  
             PA1>6 & PA1<=10 ~ "Alta"))  %>% 
  mutate(educa=
           case_when(
             PI3==1 ~ "Menor", 
             PI3==2 ~ "Similar", 
             PI3==3 ~ "Mayor")) 

tabla <- table(base$educa, base$satis)
tabla


GoodmanKruskalGamma(tabla, conf.level=0.95)

#Como el cero está dentro del intervalo no rechazamos la nula. No hay evidencia suficiente para concluir que las variables tienen una relación significativa según el coeficiente gamma.


# Tau de Kendall ----------------------------------------------------------


#La Tau de Kendall (τ) es una medida de correlación no paramétrica que evalúa la relación entre dos variables ordinales o de rango. Se basa en contar la concordancia y discordancia entre pares de observaciones, similar al coeficiente gamma de Goodman y Kruskal, pero con diferencias en el tratamiento de los empates.

#Se usa cuando:
#✅ Se tienen datos ordinales o rangos en lugar de valores numéricos continuos.
#✅ Se quiere medir la asociación entre dos variables sin asumir normalidad.
#✅ Se busca una alternativa a la correlación de Spearman, que puede ser más sensible a la distribución de los datos.

#Tau A: No ajusta empates, solo cuenta pares concordantes y discordantes.

#Tau B: Ajusta los empates en ambas variables, lo que la hace más precisa en datos con repeticiones.

#Los empates son valores repetidos en una o ambas variables, y afectan cómo se mide la correlación en datos ordinales. 

#Empates en X: Cuando x₁ = x₂ para un par de observaciones
#Empates en Y: Cuando y₁ = y₂ para un par de observaciones
#Empates en ambas variables: Cuando x₁ = x₂ y y₁ = y₂


#Es poco probable que utilicemos TauA, haremos el ejemplo con TauB.

KendallTauB(tabla, direction="row", conf.level=0.95)

#Direction row: Se evalúa la asociación entre los valores de las filas y los de las columnas, asumiendo que los valores en las filas determinan los de las columnas.

#El coeficiente es muy cercano a cero y el intervalo de confianza toca el cero por lo que no podemos rechazar la hipótesis nula. Existe la posibilidad de que el coeficiente sea cero.


# Ejercicio ---------------------------------------------------------------


#1. Calcular chi-cuadrado, Gamma y TauB para un par de variables ordinales de la base de datos. ¿Cuál es el resultado?, ¿hay una asociación entre las variables?


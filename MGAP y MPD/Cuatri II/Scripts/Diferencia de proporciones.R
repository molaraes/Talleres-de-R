# =========================================================
# Intervalos de confianza para la diferencia de proporciones
# Fecha: 11/02/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------
#Paquetes
pacman::p_load("haven", "readxl", "tidyverse")

#Directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Lectura de datos
base_marginacion <- read_excel("IMM_2020.xlsx", 
                               sheet = "IMM_2020")


# Diferencia de proporciones ----------------------------------------------
#Vamos a hacer una muestra
set.seed(1234)

muestra <- base_marginacion[sample(nrow(base_marginacion), size=1235), ]

#Vamos a comparar el analfabetismo a partir del alto y bajo grado de marginación.

#Primero hacemos modificaciones a nuestras variables.

#Volvemos a crear variable grado de marginación
muestra <- muestra %>% 
  mutate(grado=
           case_when(
             GM_2020=="Alto"~"1",
             GM_2020=="Bajo"~"0",
             GM_2020=="Medio"~"1",
             GM_2020=="Muy alto"~"1",
             GM_2020=="Muy bajo"~"0"))

#Vamos a crear una variable dummy=1 para aquellos municipios
#cuyo porcentaje de analfabetismo sea mayor igual que 
#el promedio
mean(muestra$ANALF)

muestra <- muestra %>% 
  mutate(analfabetismo = ifelse(ANALF >= 10.27287,"Analfabetismo_mayor","Analfabetismo_menor"))

table(muestra$analfabetismo)

#Cálculo de la diferencia de proporciones a mano:
#Paso 1. Calcular el estimador puntual (diferencia de proporciones)

table(muestra$analfabetismo, muestra$grado)

n1 <- 17+572 #grupo de bajo grado de marginación
n2 <- 466+180 #grupo de alto grado de marginación

#Ahora, de ambos grupos de marginación, queremos ver quienes tienen mayor analfabetismo
prop1 <- 17/n1
prop2 <- 466/n2

#Y la diferencia
diferencia <- prop1-prop2

#Paso 2. Calcular el error estándar
se <- sqrt(((prop1*(1-prop1))/n1)+((prop2*(1-prop2))/n2))

#Paso 3. Valor Z
z<- 1.96

#Paso 4. Margen de error
margen <- z*se

#Paso 5. Intervalo
inferior <- diferencia-margen
superior <- diferencia+margen

#Manera rápida
prop.test(x =c(17, 466), n =c(589,646))

#Ahora graficamos:

#Creamos base de datos
dat1 <- muestra %>%
  group_by(grado, analfabetismo) %>%
  dplyr::summarise(n = n()) %>% 
  mutate(prop = n/sum(n),
         se = sqrt(prop*(1-prop)/n))

#El gráfico
ggplot(dat1, aes(x = as.factor(grado), 
y = prop, fill = as.factor(analfabetismo))) + 
  geom_bar(stat = 'identity', 
           position = 'dodge')+
  geom_errorbar(aes(ymin = prop - se, 
                    ymax = prop + se),                            
                width = 0.2,
                position = position_dodge(0.9))


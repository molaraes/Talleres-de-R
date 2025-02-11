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

muestra <- base_marginacion[sample(nrow(base_marginacion), size=1000), ]

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
  mutate(analfabetismo = ifelse(ANALF >= 10.25291,"Analfabetismo_mayor","Analfabetismo_menor"))

table(muestra$analfabetismo)

#Cálculo de la diferencia de proporciones a mano:
#Paso 1. Calcular el estimador puntual (diferencia de proporciones)

table(muestra$analfabetismo, muestra$grado)


n1 <- 15+467 #grupo de bajo grado de marginación
n2 <- 373+145 #grupo de alto grado de marginación

#Ahora, de ambos grupos de marginación, queremos ver quienes tienen mayor analfabetismo
prop1 <- 15/n1
prop2 <- 373/n2

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
prop.test(x =c(15, 373), n =c(482,518),
          alternative=c("two.sided"))

prop.test(x =c(15, 373), n =c(482,518),
          alternative=c("greater"))

prop.test(x =c(15, 373), n =c(482,518),
          alternative=c("less"))
  
  

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


# Práctica ----------------------------------------------------------------

#Replica el ejercicio de diferencia de proporciones con la variable de grado de marginación que generamos y otra de las variables de la base de datos. Interpreta los resultados, ¿cuál es la hipótesis nula y cuál es la hipótesis alternativa?, ¿se rechaza o no se rechaza la hipótesis nula? (Hacer el ejercicio de cola izquierda, ambas colas y cola derecha).

#Un investigador afirma que la edad promedio de los migrantes internacionales que llegan a la Ciudad de México es mayor a 35 años. Con una muestra de 200 migrantes recientes, determina las hipótesis para verificar esta afirmación.

#Un estudio previo mostró que el 45% de los hogares en zonas rurales tiene acceso a internet. Un nuevo programa social afirma haber modificado este porcentaje. Con una muestra de 500 hogares rurales, establece las hipótesis para determinar si el porcentaje ha cambiado.

#La tasa de desempleo juvenil en una región se ha mantenido históricamente en 12%. Después de implementar un programa de primer empleo, se quiere probar si la tasa ha disminuido. Con datos de 300 jóvenes, establece las hipótesis.

#Las autoridades afirman que el tiempo promedio de espera para trámites en oficinas gubernamentales es igual a 45 minutos. Un grupo ciudadano sospecha que es diferente. Con una muestra de 150 usuarios, plantea las hipótesis.

#Un programa de transferencias monetarias condicionadas afirma que ha logrado reducir la deserción escolar a menos del 8% en secundarias públicas. Con datos de 400 estudiantes, establece las hipótesis para verificar esta afirmación.

#Históricamente, el 60% de las mujeres en edad reproductiva en cierta región tienen acceso a servicios de salud reproductiva. Después de una intervención gubernamental, se quiere probar si este porcentaje ha aumentado. Con una muestra de 600 mujeres, establece las hipótesis.

#Los datos oficiales indican que el ingreso promedio mensual de trabajadores informales es de $4,500 pesos. Un investigador sospecha que este dato no es preciso y podría ser diferente. Con una muestra de 250 trabajadores informales, plantea las hipótesis.




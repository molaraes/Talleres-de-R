# =========================================================
# Intervalos de confianza para la media de dos poblaciones
# Fecha: 27/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

# Paquetería
pacman::p_load("tidyverse", "Rmisc")

# Carpeta de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

# Datos
base <- read_csv("TMODULO.csv")


# Diferencia de medias para poblaciones independientes --------------------

# Supuestos clave:
#1. Las muestras son independientes.
#2. Las distribuciones de las poblaciones tienen varianzas iguales
#3. Los datos son aproximadamente normales

# El objetivo es ver la estimación puntual de la diferencia de las medias e intervalos de confianza en el tiempo dedicado al cuidado entre hombres y mujeres.

# La variable que vamos a utilizar la variable (en horas de lunes a viernes) P6_11A_09_1: Durante la semana pasada, sea en la casa, hospital u otro lugar, ¿usted lo(s) ayudó o apoyó en las tareas de la escuela o trabajo?

# Tomamos los casos válidos y convertimos la variable como numérica y el sexo como factor
base_modif <- base %>% 
  filter(!is.na(P6_11A_09_1)) %>% 
  mutate(P6_11A_09_1=as.numeric(P6_11A_09_1),
         SEXO=case_when(SEXO==1~"Hombre",
                        SEXO==2~"Mujer"))

#Paso 1. Estimador puntual (diferencia de medias)
base_modif %>% 
  group_by(SEXO) %>% 
  summarise(media=mean(P6_11A_09_1))

diferencia <- 3.11-1.80

#Paso 2. Calculamos error estándar
#Varianzas
var_sexo1 <- var(base_modif$P6_11A_09_1[base_modif$SEXO == "Hombre"])
var_sexo2 <- var(base_modif$P6_11A_09_1[base_modif$SEXO == "Mujer"])
#Tamaño de muestra
n_sexo1 <- sum(base_modif$SEXO == "Hombre")
n_sexo2 <- sum(base_modif$SEXO == "Mujer")
#Calculamos sp
numerador <- sqrt(((n_sexo1-1)*var_sexo1)+((n_sexo2-1)*var_sexo2))
denominador <-sqrt((n_sexo1+n_sexo2)-2)
sp <- numerador/denominador
#Error estándar
se <- sp*sqrt((1/n_sexo1)+(1/n_sexo2))

#Paso 3. Valor t
t<- 1.96

#Paso 4. Margen de error
margen <- t*se

#Paso 5. Construir el intervalo
inferior <- diferencia-margen
superior <- diferencia+margen

#Forma rápida
#Varianzas iguales
t.test(P6_11A_09_1 ~ SEXO,
       mu=0,
       data=base_modif,
       alternative=c("two.sided"),
       var.equal=TRUE)

#Varianzas diferentes
t.test(P6_11A_09_1 ~ SEXO,
       mu=0,
       data=base_modif,
       alternative=c("two.sided"),
       var.equal=FALSE)

#Graficamos
dat <- summarySE(base_modif, measurevar="P6_11A_09_1", groupvars="SEXO")

#Graficamos
ggplot(dat, aes(x=SEXO, y=P6_11A_09_1)) + 
  geom_errorbar(aes(ymin=P6_11A_09_1-ci, ymax=P6_11A_09_1+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=3, shape=21, fill="pink") + # 21 es círculo
  labs(
    x="Sexo",
    y="Horas de lunes a viernes",
    title="Promedio del tiempo dedicado al apoyo en la realización de tareas, por sexo", 
    caption="Elaboración propia con ENUT (219)")+
  theme_classic()



# Intervalos de confianza para diferencia de medias en muestras pareadas --------------------------

#Un intervalo de confianza para la diferencia de medias en muestras pareadas se utiliza cuando queremos analizar la diferencia promedio entre dos mediciones relacionadas, como mediciones antes y después de un tratamiento sobre el mismo grupo de individuos.

#Paquetes
install.packages("devtools") #Simplifica las tareas del desarrollo de software, desde la creación hasta la distribución de paquetes
devtools::install_github("kjhealy/socviz")
library(socviz)

#El paquete socviz fue creado por Kieran Healy como un complemento para su libro Data Visualization: A Practical Introduction.
#RColorbrewer para visualización.

#Los materiales del paquete los podemos ver aquí: https://cran.r-project.org/web/packages/socviz/socviz.pdf

#Datos
data(county_data) 


#La base de datos viene por condado-estado. Vamos a calcular la diferencia en el porcentaje de votos del partido republicano 216 y el porcentaje de votos del partido republicano en 212 para ver el efecto Trump.

#Se utiliza t.test() con el argumento paired = TRUE, ya que las observaciones "antes" y "después" corresponden a los mismos condados.

county_data <- county_data %>% 
  filter(!is.na(per_gop_2016) & !is.na(per_gop_212))

#Diferencia de medias 
t.test(county_data$per_gop_2016, county_data$per_gop_2012, paired = TRUE)

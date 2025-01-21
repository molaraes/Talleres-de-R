# =========================================================
# Gráficos para intervalos de confianza
# Fecha: 20/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

#Paquetería
pacman::p_load("haven", "readxl", "tidyverse", "Rmisc")

#El paquete Rmisc nos ayuda a realizar análisis estadísticos básicos y cálculos descriptivos, así como gráficos.

#Establecemos nuestro directorio de trabajo
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Datos")

#Abrimos nuestros datos
base <- read_csv("encig2023_01_sec1_A_3_4_5_8_9_10.csv")


# Intervalo de confianza para una media -----------------------------------

#Quitamos los "no contesta"
base_modif <- base %>% 
  filter(A1_5<99)

#Guardamos nuestra variable como numérica
base_modif$A1_5<- as.numeric(base_modif$A1_5)

#Vamos a graficar nuestro intervalo de confianza del promedio de la confianza en el Gobierno Federal, por región.

#Hacemos una variable de región a partir de lo siguiente.
#Noroeste: Baja California, Baja California Sur, Chihuahua, Durango, Sinaloa, Sonora.
#Noreste: Coahuila, Nuevo León, Tamaulipas.
#Occidente: Colima, Jalisco, Michoacán, Nayarit.
#Oriente: Hidalgo, Puebla, Tlaxcala, Veracruz.
#Centronorte: Aguascalientes, Guanajuato, Querétaro, San Luis Potosí, Zacatecas.
#Centrosur: Ciudad de México, México, Morelos.
#Suroeste: Chiapas, Guerrero, Oaxaca.
#Sureste: Campeche, Quintana Roo, Tabasco, Yucatán.

base_modif$CVE_ENT <- as.numeric(base_modif$CVE_ENT)

base_modif <- base_modif %>%
  mutate(
    region = case_when(
      CVE_ENT %in% c(2, 3, 8, 10, 25, 26) ~ "Noroeste",
      CVE_ENT %in% c(5, 19, 28) ~ "Noreste",
      CVE_ENT %in% c(6, 14, 16, 18) ~ "Occidente",
      CVE_ENT %in% c(13, 21, 29, 30) ~ "Oriente",
      CVE_ENT %in% c(1, 11, 22, 24, 32) ~ "Centro-Norte",
      CVE_ENT %in% c(9, 15, 17) ~ "Centro-Sur",
      CVE_ENT %in% c(7, 12, 20) ~ "Suroeste",
      CVE_ENT %in% c(4, 23, 27, 31) ~ "Sureste",
      TRUE ~ NA_character_
    )
  )

#La última línea (TRUE ~ NA_character_) establece NA como valor para los casos que no se ajusten a las condiciones anteriores.

#Vemos la variable
table(base_modif$region)

# Realizamos el cálculo de los intervalos:

dat <- summarySE(base_modif, measurevar="A1_5", 
                 groupvars="region")

#summarySE: Es una función del paquete Rmisc que calcula estadísticas descriptivas: Tamaño de muestra (N), Media (mean), Desviación estándar (sd), Error estándar (se), Intervalos de confianza (ci). Su salida es un nuevo data frame que organiza estas estadísticas.
#measurevar: Especifica la variable de interés.
#groupvars: Define las variables por las que se agruparán los datos antes de calcular los resúmenes estadísticos.

#Graficamos
ggplot(dat, aes(x=region, y=A1_5)) + 
  geom_errorbar(aes(ymin=A1_5-ci, ymax=A1_5+ci), 
                colour="black", width=.1) +
  geom_line() +
  geom_point(size=3, shape=21, fill="white") + # 21 es círculo
  xlab("Region") +
  ylab("Promedio en la confianza en el Gobierno Federal") +
  ggtitle("Confianza en el Gobierno Federal Por Region") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))  


# Intervalo de confianza para la proporción -------------------------------

#Eliminamos los datos perdidos
base_modif <- base %>% 
  filter(P3_2!=9) %>% 
  mutate(corrup = ifelse(P3_2 <= 3,1,0))

#Generamos una base para graficar
dat <- base_modif %>%  # Asigna el resultado del pipeline a un objeto llamado 'dat'
  group_by(corrup) %>%  # Agrupa los datos de 'base_modif' según los valores de la variable 'corrup'
  dplyr::summarise(n = n()) %>%  # Calcula el tamaño de la muestra ('n') para cada grupo definido por 'corrup'
  mutate( # Agrega nuevas columnas al data frame generado por 'summarise'
    prop = n / sum(n), # Calcula la proporción de cada grupo respecto al total
    se = sqrt(prop * (1 - prop) / n),  # Calcula el error estándar para cada proporción usando la fórmula binomial
    margen = 1.96*se # Calcula el margen de error
  )

#Gráfico
ggplot(dat, aes(x = as.factor(corrup), y = prop)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = prop - se, ymax = prop + se),                            
                width = 0.2,
                position = position_dodge(0.9))+
  theme_minimal()


# Práctica ----------------------------------------------------------------

#Un grupo de investigadores/as está evaluando la distribución del tiempo que mujeres y hombres dedican a los cuidados durante los días laborales (lunes a viernes) en México. Han solicitado tu apoyo como analista para interpretar datos de la Encuesta Nacional de Uso del Tiempo (ENUT) del INEGI y ofrecer evidencia que fundamente propuestas de políticas públicas: https://www.inegi.org.mx/programas/enut/2019/#microdatos

#1.¿Cuánto tiempo dedican, en promedio, las personas a los cuidados entre lunes y viernes? ¿Qué tan confiable es esta estimación a un nivel de confianza del 90, 95, 99? (Escoge solamente alguna de las variables relativa a los cuidados)
#2.¿Cómo interpretas los intervalos?
#3.¿Existen desigualdades de género en el tiempo dedicado a los cuidados? (Grafica las estimaciones de la variable que escogiste en el punto 1, por género. Incluye los intervalos de confianza al 95%).
#4.¿Qué proporción de la población dedica “mucho” tiempo a los cuidados? (Crea una nueva variable categórica -"Poco" o "Mucho"- basada en el tiempo dedicado a alguna variable realtiva los cuidados. Calcula el intervalo de confianza para la proporción de la población que dedica “mucho” tiempo, utilizando los niveles de confianza de 90%, 95% y 99%).
#5.¿Cómo interpretas los intervalos?
#6.¿Son diferentes los promedios del tiempo de cuidados según tu nueva variable? Grafica (a un nivel de confianza del 95%) el promedio del tiempo de cuidados según tu variable categórica.

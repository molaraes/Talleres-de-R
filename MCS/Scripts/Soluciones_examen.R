# =========================================================
# Soluciones
# Fecha: 13/02/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Paquetería --------------------------------------------------------------

pacman::p_load("tidyverse", "modeest", "RColorBrewer")


# Datos -------------------------------------------------------------------
setwd("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/MCS/Examen")

load("roc_parties.RData")
datos <- x

# Preguntas ---------------------------------------------------------------

# 2. Medidas de tendencia central
#Para r_trans
tabla1 <- datos %>% 
  summarise(Media=mean(r_trans),
            Mediana=median(r_trans),
            Moda=mfv(r_trans),
            Varianza=var(r_trans),
            Desviación =sd(r_trans))
#Para r_leaders
tabla2 <- datos %>% 
  summarise(Media=mean(r_leaders),
            Mediana=median(r_leaders),
            Moda=mfv(r_leaders),
            Varianza=var(r_leaders),
            Desviación =sd(r_leaders))
#Para r_candida
tabla3<- datos %>% 
  summarise(Media=mean(r_candida),
            Mediana=median(r_candida),
            Moda=mfv(r_candida),
            Varianza=var(r_candida),
            Desviación =sd(r_candida))

#Uno las tablas
tabla <- rbind(tabla1, tabla2, tabla3)
#Le agrego una columna
tabla$Variables <- c("r_trans", "r_leaders", "r_candida")

#3. Por país
por_pais1 <- datos %>%
  group_by(country) %>%
  summarise(Media_r_trans=mean(r_trans)) %>% 
  arrange(-Media_r_trans)

por_pais2 <- datos %>%
  group_by(country) %>%
  summarise(Media_r_leaders=mean(r_leaders)) %>% 
  arrange(-Media_r_leaders)

por_pais3 <- datos %>%
  group_by(country) %>%
  summarise(Media_r_candida=mean(r_candida)) %>% 
  arrange(-Media_r_candida)

#4. Histograma
datos %>% 
  ggplot(aes(x=r_trans))+
  geom_boxplot(color="black", fill="skyblue")+
  labs(title="Distribución del componente de transparencia",
       x="Indicador de transparencia",
       y="Frecuencia",
       caption="Fuente: elaboración propia con datos de Rodríguez (2022)"
  )+
  theme_minimal()

#5. Dispersión
datos %>% 
  ggplot(aes(x=r_trans, y=r_leaders))+
  geom_point(color="black", fill="skyblue")+
  geom_smooth(method="lm")+
  labs(title="Relación entre líderes y transparencia",
       x="Indicador de transparencia",
       y="Indicador de líderes",
       caption="Fuente: elaboración propia con datos de Rodríguez (2022)"
  )+
  theme_minimal()

# =========================================================
# Regresión logística 
# Fecha: 26/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "gtsummary", "nnet", "ggeffects", "broom", "marginaleffects", "RColorBrewer", "margins")

#Base de datos####
load("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MPD/Datos/bd_enadis_2022.RData")

#Quitamos las bases que no vamos a usar
rm(TAFRODESCENDIENTE, TAFRODESCENDIENTE_V, TCOE, TDISCAPACIDAD, TDISCAPACIDAD_V, TINDIGENA, TINDIGENA_V, TMAYOR, TMIGRANTE, TNINO, TRELIGION, TRELIGION_V, TVIVIENDA, TADOLESCENTE)


# Descriptivos ------------------------------------------------------------

#Variable dependiente: PM9_6_08 (De julio de 2021 a la fecha, ¿ha sido discriminada(o) o menospreciada(o), por ser mujer?)

TMUJER %>% 
 count(PM9_6_08)

#Vemos que 8 es "no responde". Entonces modificamos la variable

TMUJER %>% 
  mutate(disc=case_when(PM9_6_08==1~"Sí", PM9_6_08==2~"No"))->TMUJER

TMUJER$disc <- as.factor(TMUJER$disc)

#Vemos la variable
TMUJER %>% 
  count(disc)

#Variable independiente: edad
#Primero la pasamos a numérica
TMUJER$EDAD <- as.numeric(TMUJER$EDAD)
mean(TMUJER$EDAD)

#Graficamos
TMUJER %>% 
  filter(!is.na(disc)) %>% 
  ggplot(aes(x = disc, y = EDAD, fill = disc)) +
  geom_boxplot(color = "black") +
  labs(title = "Distribución de la edad por discriminación",
       x = "¿Ha sido discriminada?",
       y = "Edad") +
  scale_fill_manual(values = c("No" = "skyblue", "Sí" = "salmon")) +
  theme_minimal() +
  theme(legend.position = "none")


# Regresión logística -----------------------------------------------------

modelo_simple <- glm(formula=disc~EDAD,
                     family=binomial(link="logit"),
                     data=TMUJER)

summary(modelo_simple)

#¿Cuál es la probabilidad de que una mujer de edad promedio se haya sentido discriminada?

# Coeficientes del modelo
b0 <- -1.150106
b1 <- -0.032580

# Edad promedio
edad_promedio <- mean(TMUJER$EDAD, na.rm = TRUE)

# Cálculo de la probabilidad
numerador <- exp(b0 + (b1 * edad_promedio))
denominador <- 1 + exp(b0 + (b1 * edad_promedio))
p <- numerador/denominador
p

#Pero ahora quiero predecir la probabilidad P(y=1) para distintos valores de x
datos_predecir <- data.frame(EDAD = c(18,28,38,48,58))

predict(modelo_simple, datos_predecir, type="response" )

#Intervalos de confianza de los parámetros
confint(modelo_simple)

#Construcción de los intervalos de confianza de P(y=1) para distintos valores de x
ciTools::add_ci(datos_predecir, modelo_simple, alpha = 0.05)

#Graficamos
cplot(modelo_simple, "EDAD")

# Regresión logística con más variables independientes --------------------

#Tomamos los datos sociodemográficos de las mujeres
mujeres <- TSDEM %>% 
  dplyr::select(ID_VIV, ID_HOG, ID_PER, SEXO, EDAD, ENT, P3_10, P3_12, P3_15, NIV, P3_20, P3_21, TLOC) %>%
  dplyr::filter(SEXO==2)  %>%
  dplyr::mutate(across(4:13, as.numeric))

#P3_10: se considera afromexicana, negra, afrodescendiente
#P3_12: habla alguna lengua indígena
#NIV: nivel de escolaridad
#P3_20: estado civil
#P3_21: ocupación
#TLOC: localidad

#Unimos bases de datos de uno a muchos (left_join se queda con las observaciones de X)
base <- TMUJER %>% 
  left_join(mujeres, by=c('ID_VIV', 'ID_HOG', 'ID_PER'))

rm(mujeres, TMUJER, TSDEM)

#Variables independientes
#afrodescendiente
table(base$P3_10)

base <- base %>% 
  mutate(afro=case_when(P3_10==1~"Se considera afrodescendiente", P3_10==2~"No se considera afrodescendiente"), 
         afro = factor(afro, levels=c("Se considera afrodescendiente", "No se considera afrodescendiente")))

table(base$afro)

#escolaridad
table(base$NIV)

base <- base %>% 
  mutate(nivel=case_when(NIV==0~"Ninguno", 
                         NIV==1~"Básico",
                         NIV==2~"Básico",
                         NIV==3~"Básico",
                         NIV==4~"Medio superior",
                         NIV==5~"Medio superior",
                         NIV==6~"Medio superior",
                         NIV==7~"Medio superior",
                         NIV==8~"Superior",
                         NIV==9~"Superior",
                         NIV==10~"Superior"),
         nivel = factor(nivel, levels=c("Ninguno", "Básico", "Medio superior", "Superior")))

table(base$nivel)

#Modelo con más variables ####
modelo2 <- glm(disc ~ EDAD.x + afro + nivel, data = base, 
               family = binomial(link = logit))
summary(modelo2)

#Tabla
resultados <- as.data.frame(tidy(modelo1, conf.int = TRUE))

#Para los odds ratio
exp(coef(modelo2))
#Conforme aumente la edad, las probabilidades de sufrir discriminación disminuyen 2.73%
#Las personas que NO se consideran afrodescendientes tienen 29.07% menos probabilidades de sufrir discriminación comparado con quienes SÍ se consideran afrodescendientes.
#Las personas con educación básica tienen 33.24% menos probabilidades de sufrir discriminación comparado con personas sin educación
#Las personas con educación media superior tienen 1.17% menos probabilidades de sufrir discriminación comparado con personas sin educación
#Las personas con educación superior tienen 41.56% MÁS probabilidades de sufrir discriminación comparado con personas sin educación

tbl_regression(modelo2, exp = TRUE)

#Para los efectos marginales: 
marginaleffects::avg_slopes(modelo2, variables = "EDAD.x")

#Por cada año adicional de edad, la probabilidad de reportar discriminación disminuye en promedio 0.201 puntos porcentuales
marginaleffects::avg_slopes(modelo1, variables = "afro")
marginaleffects::avg_slopes(modelo1, variables = "nivel")

#Para las probabilidades predichas
predict1 <- ggpredict(modelo2, terms = c("EDAD.x[18:96 by = 10]", "nivel", "afro"))

#Graficar con ggplot
g <- ggplot(predict1, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet) 

#Agregar intervalos de confianza
g <- g + geom_point() +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high, 
                    width = 0.5))
#Colores
g <- g +
  scale_color_manual(
    values = c(Ninguno = "#BCD6DB",
               Básico = "#6D86A1",
               `Medio superior` = "#8A5DAA",
               Superior = "#4D004B")
  )

#Títulos de ejes
g <- g +
  labs(title="Probabilidad de sentirse discriminida \n según edad, escolaridad y afrodescendencia",
       x="Edad",
       y= "Probabilidad",
       colour= "Escolaridad",
       caption= "Elaboración propia con base en ENADIS 2022")

#Temas
g <- g+
  theme_minimal()+
  theme(plot.title=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        plot.caption= element_text(hjust=0.5))

g

#Graficar con ggeffects_lomismo
g <- ggpredict(modelo2, terms = c("EDAD.x[18:96 by = 10]", "nivel", "afro")) %>% 
  plot(connect_lines = TRUE)

g <- g +
  scale_color_manual(
    values = c(Ninguno = "#BCD6DB",
               Básico = "#6D86A1",
               `Medio superior` = "#8A5DAA",
               Superior = "#4D004B")) +
  labs(title="Probabilidad de sentirse discriminada \n según edad, escolaridad y afrodescendencia",
       x="Edad",
       y= "Probabilidad",
       colour= "Escolaridad",
       caption= "Elaboración propia con base en ENADIS 2022")+
  theme_minimal()+
  theme(plot.title=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        plot.caption= element_text(hjust=0.5))
g



# Con término de interacción ----------------------------------------------
modelo3 <- glm(disc ~ EDAD.x*afro + nivel , data = base, 
               family = binomial(link = logit))
summary(modelo3)

# Crear predicciones
pred <- ggpredict(modelo3, terms = c("EDAD.x [all]", "afro"))

# Graficar
plot(pred) +
  labs(
    x = "Edad",
    y = "Probabilidad de Discriminación",
    title = "Efectos Marginales: Edad × Afro"
  )

# Práctica ----------------------------------------------------------------

#1. Escoge otras tres variables para explicar la discriminación.
#2. Elabora análisis exploratorio y descriptivo.
#3. Estima un modelo que incluya una interacción.
#4. Grafica los resultados e interpreta.






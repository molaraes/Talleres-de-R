# =========================================================
# Regresión logística 
# Fecha: 27/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "ggeffects", "broom", "marginaleffects", "RColorBrewer", "margins", "ResourceSelection", "pscl")

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
  mutate(disc=case_when(PM9_6_08==1~1, PM9_6_08==2~0))->TMUJER

TMUJER$disc <- as.numeric(TMUJER$disc)

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
  ggplot(aes(x = as.factor(disc), y = EDAD, fill = disc)) +
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

#Paquete para generar los intervalos de predicción
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

#Modelo logístico binomial ####

modelo_multiple <- glm(disc ~ EDAD.x + afro + nivel, data = base, 
               family = binomial(link = logit))
summary(modelo_multiple)

#Los coeficientes representan el cambio en el log-odds (logaritmo de las probabilidades) por cada unidad de cambio en la variable.

#Bondad del ajuste
pR2(modelo_multiple)

#La null deviance mide qué tan bien predice un modelo "nulo" es decir, un modelo que solo incluye el intercepto (constante) 
#La del modelo simple es 25,808 y la del modelo múltiple es: 25,610  

#La residual deviance (o deviance residual) mide qué tan bien predice tu modelo completo con todas las variables predictoras incluidas. 
#La del modelo simple es 24,992 y la del modelo múltiple es: 24,490 

#Si la diferencia entre la null y residual es alta, el modelo múltiple mejora la predicción

#AIC (modelo simple): 24,996 
#AIC (modelo múltiple): 24,502 

#Prueba Hosmer
#La prueba de Hosmer-Lemeshow la utilizamos para evaluar el ajuste de un modelo de regresión logística. 
#ermite determinar si las probabilidades predichas por el modelo se ajustan bien a los datos observados.

#Hipótesis nula (H₀): el modelo se ajusta bien a los datos (no hay diferencias significativas entre lo observado y lo predicho).
#Hipótesis alternativa (H₁): el modelo no se ajusta bien a los datos.

#Paso 1. Generamos predicciones del modelo
# Predicciones del modelo
pred <- predict(modelo_multiple, type = "response")

#Paso 2. Tomamos casos válidos
# Obtenemos las observaciones usadas en el modelo
obs_modelo <- as.numeric(names(fitted(modelo_multiple)))

# Filtramos base$disc para que coincida
datos_validos <- data.frame(
  y = base$disc[obs_modelo], 
  p = pred
)

#Paso 3. Hacemos la prueba
hoslem.test(datos_validos$y, datos_validos$p)

#Tabla
resultados <- as.data.frame(tidy(modelo_multiple, conf.int = TRUE))

#Para los odds ratio
#Mide el cambio en las probabilidades relativas (odds)
#Compara: P(evento)/P(no evento)
exp(coef(modelo_multiple))

#Conforme aumente la edad, las probabilidades de sufrir discriminación disminuyen 2.73%
#Las personas que NO se consideran afrodescendientes tienen 29.07% menos probabilidades de sufrir discriminación comparado con quienes SÍ se consideran afrodescendientes.
#Las personas con educación básica tienen 33.24% menos probabilidades de sufrir discriminación comparado con personas sin educación
#Las personas con educación media superior tienen 1.17% menos probabilidades de sufrir discriminación comparado con personas sin educación
#Las personas con educación superior tienen 41.56% MÁS probabilidades de sufrir discriminación comparado con personas sin educación

#Para los efectos marginales: efecto marginal promedio de la variable edad sobre la probabilidad de discriminación.

marginaleffects::avg_slopes(modelo_multiple, variables = "EDAD.x")
#Por cada año adicional de edad, la probabilidad de reportar discriminación disminuye en promedio 0.201 puntos porcentuales
marginaleffects::avg_slopes(modelo_multiple, variables = "afro")
marginaleffects::avg_slopes(modelo_multiple, variables = "nivel")

# El comando avg_slopes() calcula el cambio promedio en la probabilidad predicha para un cambio de una unidad en la variable edad, manteniendo todas las demás variables constantes.

#Para las probabilidades predichas
predict1 <- ggpredict(modelo_multiple, terms = c("EDAD.x[18:96 by = 10]", "nivel", "afro"))

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


# Con término de interacción ----------------------------------------------
modelo_inter <- glm(disc ~ EDAD.x*afro + nivel , data = base, 
               family = binomial(link = logit))
summary(modelo_inter)

# Crear predicciones
pred <- ggpredict(modelo_inter, terms = c("EDAD.x [all]", "afro"))

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






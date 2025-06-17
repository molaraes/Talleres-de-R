# =========================================================
# Regresión multinomial 
# Fecha: 17/06/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "socviz", "nnet", "margins", "marginaleffects", "ggeffects", "scales")

#Documentos socviz
#https://cran.r-project.org/web/packages/socviz/socviz.pdf

data(gss_sm)


# Modificación variables -------------------------------------------------------------

# Vamos a analizar si existe relación entre identificación partidista y:
# - Edad
# - Género
# - Religion
# - Haber votado por Obama en la elección anterior
# - Origen étnico

# Vamos a recodificar 'partyid' para reducir las categorías a grupos principales
gss_sm <- gss_sm %>%
  mutate(identificacion = case_when(
    partyid %in% c("Strong Democrat", "Not Str Democrat", "Ind,near Dem") ~ "Demócrata",
    partyid %in% c("Independent") ~ "Independiente",
    partyid %in% c("Ind,near Rep", "Not Str Republican", "Strong Republican") ~ "Republicano",
    partyid %in% c("Other Party") ~ "Otro"
  )) %>%
  mutate(identificacion = factor(identificacion, 
                                 levels = c("Demócrata", "Independiente", "Republicano", "Otro")))

# Modificamos religion
gss_sm <- gss_sm %>%
  mutate(religion2=case_when(religion=="Protestant"~"Protestante",
                             religion=="Catholic"~"Católico",
                             religion=="Jewish"~"Otra",
                             religion=="None"~"Ninguna",
                             religion=="Other"~"Otra")) %>% 
  mutate(religion2 = factor(religion2, levels=c("Protestante", "Católico", "Ninguna", "Otra")))

# Modificamos voto por Obama
gss_sm %>% 
  mutate(obama2=case_when(obama==1~"Votó por Obama en 2012", 
                         obama==0~"No votó por Obama en 2012"))->gss_sm


# Descriptivo bivariado ---------------------------------------------------

#ID y religión
gss_sm %>% 
 tabyl(identificacion, religion2)
#ID y género
gss_sm %>% 
  tabyl(identificacion, sex)
#ID y voto por Obama 
gss_sm %>% 
  tabyl(identificacion, obama)
#ID y origen étnico
gss_sm %>% 
  tabyl(identificacion, race)
#ID y edad
gss_sm %>% 
  filter(!is.na(identificacion)) %>% 
  filter(!is.na(age)) %>% 
  ggplot(aes(x = identificacion, y = age, fill = identificacion)) +
  geom_boxplot(color = "black")

# Regresión multinomial ---------------------------------------------------

#Estimamos un modelo de regresión multinomial
#La función se llama multinom (paquete nnet)

#No incluye p valores

#Podemos cambiar la categoría de referencia
gss_sm$identificacion <- relevel(gss_sm$identificacion, ref="Demócrata")

#Estimar modelo
modelo <- multinom(identificacion ~ religion2 + sex + obama2 + race + age, data=gss_sm)
summary(modelo)

#Podemos calcular odds ratio
exp(coef(modelo))

#Intervalos de confianza
confint(modelo)

#Pvalores
#Calculamos nuestro valor z
z <- summary(modelo)$coefficients/summary(modelo)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p

#Para los efectos marginales: efecto marginal promedio de la variable edad sobre la probabilidad de identificación partidista.

marginaleffects::avg_slopes(modelo, variables = "age")
#Por cada año adicional de edad, la probabilidad de identificarse como demócrata aumenta en 0.097 puntos porcentuales
marginaleffects::avg_slopes(modelo, variables = "religion2")
marginaleffects::avg_slopes(modelo, variables = "sex")
marginaleffects::avg_slopes(modelo, variables = "race")
marginaleffects::avg_slopes(modelo, variables = "obama2")

# El comando avg_slopes() calcula el cambio promedio en la probabilidad predicha para un cambio de una unidad en la variable independiente, manteniendo todas las demás variables constantes.

#Para las probabilidades predichas
# Crear predicciones con marginaleffects
predict1 <- predictions(modelo,
newdata = datagrid(religion2 = unique(gss_sm$religion2),
                   obama2 = unique(gss_sm$obama2),
sex = "Female",  # Valor fijo
race = "White", # Valor fijo
age=49.15576)) # Valor fijo

#Graficamos
predict1 %>% 
  filter(!is.na(estimate)) %>%
  ggplot(aes(x = religion2, y = estimate, color = factor(obama2))) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  # Puntos de tamaño 3, separados horizontalmente 0.3 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
  # Barras de error verticales usando los IC
  width = 0.2, position = position_dodge(width = 0.3)) +
  # Ancho 0.2 y misma separación horizontal que los puntos
  facet_wrap(~ group, 
  # Crea paneles separados para cada categoría de ID
             labeller = labeller(group = function(x) paste("Identificación:", x))) +
  # Personaliza los títulos de cada panel 
  scale_x_discrete(name = "Religión") +
  # Configura el eje X como discreto y le pone título "Religión"
  scale_y_continuous(name = "Probabilidad predicha",
  # Configura el eje Y como continuo con título "Probabilidad     predicha"
  labels = percent_format(accuracy = 1)) +
  # Formatea las etiquetas del eje Y como porcentajes sin decimales
  scale_color_brewer(name = "Voto por Obama", 
  # Usa paleta de colores ColorBrewer con título "Voto por Obama"
  type = "qual", 
  # Especifica que es una variable cualitativa
                     palette = "Set1") +
  # Usa la paleta "Set1" de ColorBrewer
  theme_minimal() +
  # Aplica el tema minimalista 
  theme(
    # Personaliza elementos específicos del tema
    strip.text = element_text(face = "bold", size = 11),
    # Títulos de facetas en negrita y tamaño 11
    legend.position = "bottom",
    # Leyenda en la parte inferior
    panel.grid.minor = element_blank(),
    # Elimina las líneas de cuadrícula menores
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    # Título centrado, en negrita y tamaño 14
    plot.subtitle = element_text(hjust = 0.5, color = "gray60"),
    # Subtítulo centrado y en color gris
    legend.title = element_text(face = "bold"),
    # Título de la leyenda en negrita
    axis.text.x = element_text(angle = 45, hjust = 1)
    # Etiquetas del eje X rotadas 45° y alineadas a la derecha
  ) +
  labs(
    title = "Probabilidades predichas de identificación partidista",
    subtitle = "Por religión y voto sobre Obama (Mujeres, blancas, edad promedio)\nIntervalos de confianza al 95%",
    caption = "Fuente: Modelo de regresión multinomial"
  )

# =========================================================
# Repaso chi-cuadrada
# Fecha: 20/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "socviz", "RColorBrewer")

#Documentos socviz
#https://cran.r-project.org/web/packages/socviz/socviz.pdf


# Repaso teoría -----------------------------------------------------------

# La distribución Chi-Cuadrada (χ²) es una distribución de probabilidad con las siguientes características:
# - Notación: X ~ χ²(k), donde k representa los grados de libertad.
# - Dominio: Solo toma valores positivos (x ≥ 0).
# - Forma: Es asimétrica, sesgada a la derecha. A medida que aumentan los grados de libertad, se vuelve más simétrica y se aproxima a una distribución normal.

# La prueba de independencia Chi-Cuadrada nos permite determinar si existe una relación estadísticamente significativa entre dos variables categóricas.
# - H0 (Hipótesis nula): Las variables son estadísticamente independientes
# - H1 (Hipótesis alternativa): Las variables están relacionadas (no son independientes)

# Se dice que las variables son independientes si las distribuciones condicionales de la población son idénticas a las distribuciones marginales.


# Datos -------------------------------------------------------------------

# Vamos a analizar si existe relación entre:
# - Religion
# - Identidad partidista

data(gss_sm)
table(gss_sm$religion)
table(gss_sm$partyid)

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


# Análisis descriptivo ----------------------------------------------------

tabla_contingencia<-table(gss_sm$religion2, gss_sm$identificacion)
tabla_contingencia

# Visualización de los datos
gss_sm %>% 
  filter(!is.na(religion2)) %>% 
  filter(!is.na(identificacion)) %>% 
ggplot(aes(x = religion2, fill = identificacion)) +
  geom_bar(position = "stack") +
  labs(
    title = "Identificación partidista por religión",
    x = "Religión",
    y = "Frecuencia",
    fill = "Identificación partidista"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


# Cálculo chi cuadrado ----------------------------------------------------

options(scipen=999)
resultados_chi <- chisq.test(tabla_contingencia, correct=F)
resultados_chi

#Estadístico Chi-cuadrado (X-squared = 126.23): es muy alto, lo que sugiere una desviación entre las frecuencias observadas y las frecuencias esperadas si las variables fueran independientes.
#Grados de libertad (df = 9): estamos analizando una tabla con múltiples categorías. 
#Valor p (p-value < 0.00000000000000022): Este valor es muy pequeño (prácticamente cero), dado que el valor p es  menor que el umbral de significancia (0.05), rechazamos  la hipótesis nula que establece que no hay relación entre la identificación partidista y la religión.
#Existe una asociación clara: La afiliación religiosa de una persona y su identificación partidista no son independientes, sino que están relacionadas.

# Podemos extraer elementos
estadistico_chi <- round(resultados_chi$statistic, 2)
grados_libertad <- resultados_chi$parameter
valor_p <- resultados_chi$p.value
valor_critico <- qchisq(0.95, df = grados_libertad)  # Nivel de significancia 0.05

# Visualización -----------------------------------------------------------

# Creamos datos para la distribución
x <- seq(0, max(estadistico_chi * 1.2, 30), length.out = 1000)
y <- dchisq(x, df = grados_libertad)
datos_dist <- data.frame(x = x, y = y)


# Gráfico de la distribución con zona de rechazo
ggplot(datos_dist, aes(x = x, y = y)) +
  geom_line(linewidth = 1) +
  geom_area(data = subset(datos_dist, x > valor_critico), 
            aes(x = x, y = y), fill = "red", alpha = 0.3) +
  geom_vline(xintercept = valor_critico, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = estadistico_chi, linetype = "solid", color = "red") +
  annotate("text", x = valor_critico, y = max(y) * 0.8, 
           label = paste("Valor crítico\n", round(valor_critico, 2)), 
           hjust = -0.1, color = "blue") +
  annotate("text", x = estadistico_chi, y = max(y) * 0.6, 
           label = paste("Estadístico\nobservado\n", estadistico_chi), 
           hjust = 1.1, color = "red") +
  annotate("text", x = max(x) * 0.8, y = max(y) * 0.8, 
           label = paste("Zona de rechazo\nα = 0.05"), 
           color = "red") +
  labs(
    title = "Distribución Chi-cuadrado con grados de libertad = 9",
    subtitle = paste("Estadístico = ", estadistico_chi, 
                     ", Valor crítico = ", round(valor_critico, 2), 
                     ", p-valor < ", format(valor_p, scientific = TRUE, digits = 3)),
    x = "Valor Chi-cuadrado",
    y = "Densidad"
  ) +
  theme(
    plot.subtitle = element_text(size = 8)
  )


# Práctica ----------------------------------------------------------------

#1. Escoge otro par de variables y replica.

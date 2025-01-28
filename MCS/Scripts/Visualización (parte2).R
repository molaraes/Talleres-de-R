# =========================================================
# Visualización
# Fecha: 30/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

# Paquetería --------------------------------------------------------------
#Cargamos paquetes
pacman::p_load("tidyverse", "socviz", "RColorBrewer")
#Datos
data(organdata)
data(gss_sm)

# Último gráfico que hicimos
gss_sm %>%
  filter(!is.na(religion)) %>%  # Filtra por datos válidos
  count(religion) %>%           # Calcula las frecuencias de cada religión
  ggplot(aes(x = religion, y = n, fill = religion)) +  # Relleno por religión
  geom_bar(stat = "identity") +  # Gráfico de barras con valores de "n"
  scale_fill_brewer(palette = "Set2") +  # Aplica la paleta Set2 de RColorBrewer
  labs(
    title = "Distribución de Religión en la Región Noreste",  # Título del gráfico
    subtitle = "Frecuencia de religiones en el conjunto de datos GSS",  # Subtítulo
    x = "Religión",  # Etiqueta para el eje X
    y = "Frecuencia",  # Etiqueta para el eje Y
    caption = "Fuente: GSS Survey (Socviz)"  # Fuente de los datos
  ) +
  geom_text(aes(label = n), vjust = -0.5) +  # Agrega las etiquetas de las frecuencias
  theme_minimal() +  # Tema minimalista
  theme(legend.position = "none") +  # Elimina la leyenda
  scale_y_continuous(breaks = seq(0, 1400, 200))  # Saltos en eje Y de 200


# Gráfico de barras apiladas ----------------------------------------------

# ¿Qué pasa si queremos analizar la religión, pero entre hombres y mujeres?

# Podemos hacer un gráfico de barras apiladas, el cual analiza dos variables al mismo tiempo: por cada columna de la religión, dividiremos la barra en dos: hombres y mujeres.

gss_sm %>%
  filter(!is.na(religion) & !is.na(sex)) %>%  # Filtra datos válidos para ambas variables
  count(religion, sex) %>%  # Calcula frecuencias agrupadas por religión y sexo
  ggplot(aes(x = religion, y = n, fill = sex)) +  # Apila por sexo
  geom_bar(stat = "identity", position = "stack") +  # Gráfico de barras apiladas
  scale_fill_brewer(palette = "Set2") +  # Paleta de colores para los sexos
  labs(
    title = "Distribución de Religión por Sexo",  # Título del gráfico
    subtitle = "Frecuencia de religiones desglosada por sexo",  # Subtítulo
    x = "Religión",  # Etiqueta para el eje X
    y = "Frecuencia",  # Etiqueta para el eje Y
    caption = "Fuente: GSS Survey (Socviz)",  # Fuente de los datos
    fill = "Sexo"  # Leyenda para los colores
  ) +
  theme_minimal() +  # Tema minimalista
  scale_y_continuous(breaks = seq(0, 1400, 200))  # Saltos en eje Y de 200

# ¿Y qué pasa si quiero los porcentajes en lugar de las frecuencias absolutas?

gss_sm %>%
  filter(!is.na(religion) & !is.na(sex)) %>%  # Filtra datos válidos
  count(religion, sex) %>%  # Calcula frecuencias agrupadas por religión y sexo
  group_by(religion) %>%  # Agrupa por religión
  mutate(percentage = n / sum(n) * 100) %>%  # Calcula el porcentaje por religión
  ggplot(aes(x = religion, y = percentage, fill = sex)) +  # Usa el porcentaje como eje Y
  geom_bar(stat = "identity", position = "stack") +  # Gráfico de barras apiladas
  scale_fill_brewer(palette = "Set2") +  # Paleta de colores para los sexos
  labs(
    title = "Distribución de Religión por Sexo",  # Título del gráfico
    subtitle = "Porcentaje de religiones desglosado por sexo",  # Subtítulo
    x = "Religión",  # Etiqueta para el eje X
    y = "Porcentaje",  # Etiqueta para el eje Y
    caption = "Fuente: GSS Survey (Socviz)",  # Fuente de los datos
    fill = "Sexo"  # Leyenda para los colores
  ) +
  theme_minimal() +  # Tema minimalista
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     breaks = seq(0, 100, 25))  # Eje Y en porcentaje

#¿Y qué pasa si quiero el sexo en el eje x y que más bien las barras se apilen por religión?

#También, hay otra opción que es poner position="dodge" en lugar de stack. Esto hará que las barras estén a la par y no apiladas.


# Gráfico de dispersión ---------------------------------------------------

# ¿Los países ricos tienen una mayor tasa de donadores de órganos? Vamos a ver la relación mediante un diagrama de dispersión.

# Tres cosas clave de un diagrama de dispersión: nos dice si hay una relación lineal, nos dice si es positiva o negativa, nos dice si hay datos atípicos.

organdata %>% 
  ggplot(aes(x=gdp, y=donors))+
  geom_point(color="midnightblue", size=2)+
  labs(title="Relación entre donación de órganos y PIB",
       y="Tasa de donadores por cada millón",
       x="Producto interno bruto en dólares",
       caption="Fuente: elaboración propia con datos de Socviz (Healy)"
  )+
  theme_classic()

# Podemos agregar una recta imaginaria
organdata %>% 
  ggplot(aes(x = gdp, y = donors)) +
  geom_point(color = "midnightblue", size = 2) + # Agregamos los puntos en color azul
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) + # Agregamos la línea de regresión (roja) con intervalo de confianza
  labs(
    title = "Relación entre donación de órganos y PIB",
    y = "Tasa de donadores por cada millón",
    x = "Producto interno bruto en dólares",
    caption = "Fuente: elaboración propia con datos de Socviz (Healy)"
  ) +
  theme_classic() # Usamos un tema clásico para el gráfico

# Podemos agregar una variable categórica que rellene de color los puntos.

# ¿Cómo varía el asunto de la donación entre los diferentes regímenes de bienestar?

#El concepto de "Welfare state world" proviene del trabajo del sociólogo danés Esping-Andersen. Clasifica los estados de bienestar en tres grandes modelos o "regímenes", basados en cómo los países combinan la provisión de bienestar entre el estado, el mercado y la familia. Estos regímenes son: liberal, conservador y socialdemócrata.

organdata %>% 
  filter(!is.na(world)) %>% 
  ggplot(aes(x = gdp, y = donors, fill = world)) + # Mapear la variable "world" al relleno
  geom_point(color = "black", shape = 21, size = 3) + # Puntos con borde negro y relleno según "world"
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) + # Línea de regresión con intervalo de confianza
  labs(
    title = "Relación entre donación de órganos y PIB",
    y = "Tasa de donadores por cada millón",
    x = "Producto interno bruto en dólares",
    caption = "Fuente: elaboración propia con datos de Socviz (Healy)",
    fill="Régimen"
  ) +
  theme_classic() + # Tema clásico
  scale_fill_brewer(palette = "Set2") # Paleta de colores para los valores de "world"


# Otras funciones de manipulación de datos --------------------------------

#https://es.r4ds.hadley.nz/05-transform.html
#https://rstudio.github.io/cheatsheets/translations/spanish/data-wrangling_es.pdf

# Con group_by() y summarize() agrupamos las filas de acuerdo a una cierta columna y calculamos una métrica sobre esos grupos.

# Agrupamos los datos por region (region) y calculamos el promedio de la edad.

gss_sm %>%                               # Inicia con el conjunto de datos gss_sm
  select(region, age) %>%                # Selecciona solo las columnas "region" y "age"
  group_by(region) %>%                   # Agrupa los datos por la columna "region"
  summarize(Edad_prom = mean(age, na.rm = T)) %>%  # Calcula la media de "age" por cada "region", ignorando los NA (na.rm = T)
  ungroup()                             # Elimina el agrupamiento, para que los datos no estén agrupados después de la operación

# Crear una variable dummy
gss_sm2 <- gss_sm %>% 
  mutate(religion3=ifelse(religion=="Catholic", "Catolicismo", "Otra"))

# Crear una variable con más de dos categorías
gss_sm2 <- gss_sm %>% 
  mutate(religion2=case_when(religion=="Protestant"~"Protestante",
                             religion=="Catholic"~"Católico/a",
                             religion=="Jewish"~"Judío/a",
                             religion=="None"~"Ninguna",
                             religion=="Other"~"Otra"))




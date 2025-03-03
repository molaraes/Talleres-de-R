# =========================================================
# Visualización
# Fecha: 23/01/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================

Cheatsheet en español: 
https://diegokoz.github.io/intro_ds/fuentes/ggplot2-cheatsheet-2.1-Spanish.pdf

El ggplot2 se basa en la construcción de gráficos a partir de tres componentes:
1) Datos, 
2) Coordenadas y 
3) Objetos geométricos

Esto será nuestra "gramática de gráficas"

Para visualizar los resultados, nosotres asignamos variables a las propiedades 
visuales o estéticas

Por ejemplo: los tamaños, colores y posiciones.

De manera genérica, podríamos pensar que el código para el ggplot será 
de la siguiente manera:
ggplot(datos)+(geometria)+(esteticas)


# Paquetería --------------------------------------------------------------
install.packages("devtools") #Simplifica las tareas del desarrollo de software, desde la creación hasta la distribución de paquetes
devtools::install_github("kjhealy/socviz")

#Cargamos paquetes
install.packages("pacman") #instalar pacman
pacman::p_load("tidyverse", "socviz", "RColorBrewer")
#Tidyverse para manipulación de datos
#El paquete socviz fue creado por Kieran Healy como un complemento para su libro Data Visualization: A Practical Introduction.
#RColorbrewer para visualización.


#Los materiales del paquete los podemos ver aquí: https://cran.r-project.org/web/packages/socviz/socviz.pdf

# Gráficos para variables cuantitativas -----------------------------------
# Cargar los datos organdata en el entorno
data(organdata)

# Utilizaremos el paquete ggplot2 para realizar visualizaciones. Ggplot2 forma parte del conjunto de paquetes de tidyverse.

# Histograma:

# Haremos un histograma del gasto en salud (en miles de dólares)
organdata %>% # tabla donde están los datos
  ggplot(aes(x=health)) +  # variable(s) que queremos
  geom_histogram()                         # geometría

# Queda un histograma estándar (medio feo).

# Podemos personalizar algunos elementos.
organdata %>%
  ggplot(aes(x=health)) +
  geom_histogram(color="black",fill="steelblue")+ # contorno negro y relleno steelblue
  labs(title="Distribución del gasto en salud per capita en los países de la OECD, 1991-2002", #título de gráfico
       x="Gasto en salud (en miles de dólares) per cápita", #eje x
       y="Frecuencias", #eje y
       caption="Fuente: Data Visualization(Healy, 2018)") + #fuente
  theme_minimal()

# Queda un histograma (distribución) un poco sesgada.
# Se ve mejor la estructura de los datos si hacemos un histograma con el logaritmo de la variable.
organdata %>%
  ggplot(aes(x=log(health))) +
  geom_histogram(color="black",fill="steelblue")+ # contorno negro y relleno steelblue
  labs(title="Distribución del gasto en salud per capita en los países de la OECD, 1991-2002", #título de gráfico
       x="Gasto en salud (en miles de dólares) per cápita", #eje x
       y="Frecuencias", #eje y
       caption="Fuente: Data Visualization(Healy, 2018)") + #fuente
  theme_minimal()

#¿Cómo se verá en un diagrama de caja y bigotes? 
#Podemos tomar el mismo código y cambiar la geometría.
organdata %>%
  ggplot(aes(x=log(health))) +
  geom_boxplot(color="black",fill="steelblue")+ # contorno negro y relleno steelblue
  labs(title="Distribución del gasto en salud per capita en los países de la OECD, 1991-2002", #título de gráfico
       x="Gasto en salud (en miles de dólares) per cápita", #eje x
       y="Frecuencias", #eje y
       caption="Fuente: Data Visualization(Healy, 2018)") + #fuente
  theme_minimal()


# Gráficos para variables cualitativas------------------------------------

# La función filter toma una tabla y nos devuelve sólo las filas que cumplen una cierta condición. La función select() de dplyr toma una tabla y devuelve sólo las columnas que nosotros le digamos. La función arrange() ordenas las filas de una tabla de acuerdo a los valores de una cierta columna.

# Cargar los datos gss_sm en el entorno
data("gss_sm") #Podemos ver de qué trata la base en el pdf.

gss_sm %>% # Toma la tabla
  filter(bigregion=="Northeast") %>% # Filtra 
  select(id, religion, partyid, zodiac) %>% # Selecciona las comunas 
  arrange(id)    # Ordena de menor a mayor (si fuera de mayor a menor se pone un menos antes de la variable)

# Ahora hagamos un gráfico de barras
gss_sm %>%
  filter(!is.na(religion)) %>% #Filtra por datos válidos
  count(religion) %>% # Calcula las frecuencias de cada religión
  ggplot(aes(x = religion, y = n)) +
  geom_bar(stat = "identity") 

# Le agregamos color
gss_sm %>%
  filter(!is.na(religion)) %>% #Filtra por datos válidos
  count(religion) %>% # Calcula las frecuencias de cada religión
  ggplot(aes(x = religion, y = n, fill=religion)) + #Relleno 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") + # Aplica la paleta Set2 de RColorBrewer
  theme_minimal()

# Le agregamos etiquetas
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
    caption = "Fuente: GSS Survey (Socviz)",  # Fuente de los datos
    fill = "Religión"  # Leyenda para el color
  ) +
  geom_text(aes(label = n), vjust = -0.5) +  # Agrega las etiquetas de las frecuencias
  theme_minimal()  # Tema minimalista


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




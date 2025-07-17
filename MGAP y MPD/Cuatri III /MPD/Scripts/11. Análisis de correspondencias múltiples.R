# Análisis de Correspondencias Mútiples ----
# Autor: Gerardo Damián
#
# Taller de Estadística III - Maestría en Población y Desarrollo FLACSO México. Titular: Dra. Mónica Lara Escalante
# 17-07-2024


## Paquetes ----
library(pacman)

p_load(janitor,
       FactoMineR,
       tidyverse,
       factoextra,
       broom,
       gridExtra,
       GGally,
       ca)

# Bases del ACM ----
# Analiza relaciones entre múltiples variables categóricas:
#       patrones, afinidades, asociaciones
#
# Se basa en frecuencias y distancias chi-cuadrado,
#       no en relaciones lineales entre variables numéricas
#
# Representa las relaciones en un plano multidimensional: 
#       reducción de dimensiones facilita la interpretación 
## Interpretación visual: mapa factorial
#       muestra categorías (individuos, o ambos) para analizar
#       agrupamientos, asociaciones,
#       oposiciones y ejes de diferenciación
#
# Funciona bajo una lógica intuitiva

## Requisitos y supuestos ----
# Utilizar variables categóricas: nominales u ordinales
# Más de 3 variables
# No requiere verificar supuestos como otras técnicas
#      (normalidad, linealidad, homocedasticidad, multicolinealidad...)
# Atención a categorías vacías (generan problemas en los cálculos)
# Considera  muestra "grande" 
#       al menos más de 100 casos
#       regla informal: tener más observaciones que categorías
#       RECOMENDACIÓN: que las variables utilizadas tengan un 
#       mismo número de categorías (no siempre se puede cumplir)

# Utilidad ----
# Analizar perfiles:
#   patrones de consumo
#   perfiles socioeconómicos
#   perfiles demográficos
#      (reproductivos, migratorios, conyugales, laborales, educativos)
#   actitudes
#   participación ciudadana
#   preferencias electorales
#   confianza en instituciones
#   patrones y estilos de vida
#   segmentación
#   tipologías de capacidad institucional
#   identificar de problemáticas sociales y políticas relacionadas entre sí

# Ventajas y desventajas ----
# 
# |       Ventajas          |      Desventajas
# | Muestra asociaciones    | No muestra causalidad
# |_________________________|_____________________________________________
# | Intuitivo y flexible    | Sensibilidad a pocas 
# |                         |frecuencias y al tamaño de muestra
# |_________________________|_____________________________________________
# | Multidimensionalidad    | Requiere interpretación teórica
# |                         | (los ejes no dicen nada por sí solos)
# |_________________________|_____________________________________________
# | Combina con técnicas    | Complejidad para interpretar
# | (estadísticas) y        | al tener muchas categorías
# | enfoques (cualitativos) |
# |_________________________|_____________________________________________

# Combinación con técnicas y enfoques:
## Explorar y confirmar tipos: 

#   ACM -> Clases Latentes
#      Equivalente a PCA -> Análisis factorial

## Identificar categorías de individuos
#   ACM -> Clústers         

## Análisis estadísticos mixtos
#  Análisis factorial  + ACM
#   vars. continuas      var. categóricas
#
#  Elaboración de índices
#
#  ACM -> Técnicas cualitativas
#         para profundizar en perfiles, tipos

## Diferencias con Análisis de Correspondencias Simples (AC) ----
# En AC se usan 2 variables

# En ACM: 3 o más (relaciones multidimensionales)


# Flujo de trabajo ----
#   1. Preprar los datos
#      1.1 Codificar en categorías binarias:
#          R lo hace en automático
#   2. Explorar los datos
#      2.1 Categorías con menos de 1%: reagruparlas
#     2.2 Atención a categorías vacías: borrarlas o 
#          tratarlas como una categoría en sí
#   3. Realizar ACM
#      3.1 Tabla de combinación de categorías
#          y cálculo de distancias
#      3.2 Reducción dimensional: 
#          analizar la varianza explicada
#          de los ejes, ¿cómo resumen la información original?
#      3.3 Corregir medidas: opcional pero recomendado
#      3.4 Seleccionar ejes (dimensiones) a utilizar
#      3.5 Elaborar y analizar mapas factoriales
#      3.6 Análisis
#   5. Interpretación y presentación de resultados

# Abrir bases ----

# ENBIARE 2021: preguntas sobre bienestar (TENBIARE) y datos sociodemográficos

base1 <- read_csv("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MGAP/Datos/TENBIARE.csv",
                  col_select = c("FOLIO",
                                 "VIV_SEL",
                                 "HOGAR",
                                 "N_REN",
                                 "PA1",
                                 "PA2",
                                 "PA3_01",
                                 "PA3_02",
                                 "PA3_03",
                                 "PA3_04",
                                 "PA3_05",
                                 "PA3_06",
                                 "PA3_07",
                                 "PB1_01",
                                 "PB1_02",
                                 "PB1_03",
                                 "PB1_04",
                                 "PB1_05",
                                 "PB1_06",
                                 "PB1_07",
                                 "PE1",
                                 "PE2",
                                 "PE5"))

basedem <- read_csv("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/Taller estadística/Estadística 3/MGAP/Datos/TSDEM.csv",
                    col_select = c("FOLIO",
                                   "VIV_SEL",
                                   "HOGAR",
                                   "N_REN",
                                   "SEXO",
                                   "NIVEL",
                                   "PAREN",
                                   "P4_9",
                                   "EDAD"))

#### Limpiar nombres

base1<- clean_names(base1)
basedem<- clean_names(basedem)

# Verificar nombres en minúsculas

names(base1)
names(basedem)

### Pegar bases ----
# left_join para mantener las observaciones de la TENBIARE

base <- left_join(base1,basedem, by = c("folio","viv_sel","hogar", "n_ren"))

# borra bases que no se usan
rm(base1,basedem)

# Preparar los datos ----

# Los análisis de correspondencias se realizan con variables categóricas. 
# Tenemos que cerciorarnos que las variables que vamos a utilizar están
#     etiquetadas y sean factores

base <- base %>%
  mutate(across(c(pa1,
                  pa2,
                  pa3_01,
                  pa3_02,
                  pa3_03,
                  pa3_04,
                  pa3_05,
                  pa3_06,
                  pa3_07,
                  pb1_01,
                  pb1_02,
                  pb1_03,
                  pb1_04,
                  pb1_05,
                  pb1_06,
                  pb1_07),
                as.numeric))

# Variables sociodemográficas
base <- base %>%
  mutate(
    sexo = factor(recode(sexo,
                         "1" = "Hombre",
                         "2" = "Mujer"),
                  levels = c("Hombre", "Mujer")),
    nivel = factor(case_when(
      nivel %in% c("00", "01", "02", "03") ~ "Básico",
      nivel %in% c("04", "05", "06", "07") ~ "Medio superior", 
      nivel %in% c("08", "09", "10") ~ "Superior y Posgrado", 
      nivel == "99" ~ "Sin información",
      is.na(nivel) ~ "Sin información",  
      TRUE ~ "Sin información"  
    ), levels = c("Básico", "Medio superior", "Superior y Posgrado", "Sin información")),
    paren = factor(recode(paren,
                          "1" = "Jefa(a)",
                          "2" = "Esposo(a)",
                          "3" = "Hijo(a)",
                          "4" = "Nieto(a)",
                          "5" = "Yerno/Nuera",
                          "6" = "Padre,Madre, Suegro(a)",
                          "7" = "Otro parentesco",
                          "8" = "Sin parentesco"),
                   levels = c("Jefa(a)", "Esposo(a)", "Hijo(a)", "Nieto(a)",
                              "Yerno/Nuera", "Padre,Madre, Suegro(a)",
                              "Otro parentesco", "Sin parentesco")),
    p4_9 = factor(recode(p4_9,
                         "1" = "Unión libre",
                         "2" = "Separación",
                         "3" = "Divorcio",
                         "4" = "Viudez",
                         "5" = "Matrimonio",
                         "6" = "Soltería",
                         "9" = "No sabe"),
                  levels = c("Unión libre", "Separación", "Divorcio", "Viudez",
                             "Matrimonio", "Soltería", "No sabe")))

# Variables laborales
base <- base %>%
  mutate(
    pe1 = factor(recode(pe1,
                        "1" = "Empleo",
                        "2" = "Negocio",
                        "3" = "Cuenta propia",
                        "4" = "Ninguna"),
                 levels = c("Empleo", "Negocio", "Cuenta propia", "Ninguna")),
    pe2 = factor(recode(pe2,
                        "1" = "Sí",
                        "2" = "No"),
                 levels = c("Sí", "No")),
    pe5 = factor(recode(pe5,
                        "1" = "Buscar trabajo país",
                        "2" = "Buscar trabajo extranjero",
                        "3" = "Empezar negocio",
                        "4" = "No ha buscado trabajo"),
                 levels = c("Buscar trabajo país", "Buscar trabajo extranjero", 
                            "Empezar negocio", "No ha buscado trabajo")))


# Identificar: ocupación, desocupación y PNEA
base <- base %>%
  mutate(
    pea_status = case_when(
      pe2 == "Sí" ~ "Ocupado",
      is.na(pe2) & pe5 %in% c("Buscar trabajo país", "Buscar trabajo extranjero", "Empezar negocio") ~ "Desocupado",
      is.na(pe2) & pe5 == "No ha buscado trabajo" ~ "No económicamente activa",
      TRUE ~ "Sin información"),
    pea_status = factor(pea_status,
                        levels = c("Ocupado", "Desocupado", "No económicamente activa", "Sin información")))

# Variables numéricas (edad) se convierten en categóricas (grupos de edad)

base <- base %>% 
  mutate(gpo_edad = case_when(
    edad%in%c(18:29) ~ "18 a 29",
    edad%in%c(30:49) ~ "30 a 49",
    edad%in%c(50:69) ~ "50 a 69",
    edad%in%c(70:99) ~ "+70"))

base$gpo_edad <- factor(base$gpo_edad, levels = c("18 a 29",
                     "30 a 49",
                     "50 a 69",
                     "+70"))

# Variables de satisfación y confianza son escalas de 0 a 10
# es mejor convertirlas en categóricas 

base <- base %>%
  mutate(
    satisf_pa1 = case_when(
      pa1 %in% as.character(0:3) ~ "Baja satisfacción",
      pa1 %in% as.character(4:7) ~ "Media satisfacción",
      pa1 %in% as.character(8:10) ~ "Alta satisfacción"
    ),
    
    satisf_pa2 = case_when(
      pa2 %in% 0:3 ~ "Baja satisfacción",
      pa2 %in% 4:7 ~ "Media satisfacción",
      pa2 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_01 = case_when(
      pa3_01 %in% 0:3 ~ "Baja satisfacción",
      pa3_01 %in% 4:7 ~ "Media satisfacción",
      pa3_01 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_02 = case_when(
      pa3_02 %in% 0:3 ~ "Baja satisfacción",
      pa3_02 %in% 4:7 ~ "Media satisfacción",
      pa3_02 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_03 = case_when(
      pa3_03 %in% 0:3 ~ "Baja satisfacción",
      pa3_03 %in% 4:7 ~ "Media satisfacción",
      pa3_03 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_04 = case_when(
      pa3_04 %in% 0:3 ~ "Baja satisfacción",
      pa3_04 %in% 4:7 ~ "Media satisfacción",
      pa3_04 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_05 = case_when(
      pa3_05 %in% 0:3 ~ "Baja satisfacción",
      pa3_05 %in% 4:7 ~ "Media satisfacción",
      pa3_05 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_06 = case_when(
      pa3_06 %in% 0:3 ~ "Baja satisfacción",
      pa3_06 %in% 4:7 ~ "Media satisfacción",
      pa3_06 %in% 8:10 ~ "Alta satisfacción"
    ),
    
    satisf_pa3_07 = case_when(
      pa3_07 %in% 0:3 ~ "Baja satisfacción",
      pa3_07 %in% 4:7 ~ "Media satisfacción",
      pa3_07 %in% 8:10 ~ "Alta satisfacción"
    )
  )

base <- base %>%
  mutate(
    conf_pb1_01 = case_when(
      pb1_01 %in% 0:3 ~ "Baja confianza",
      pb1_01 %in% 4:7 ~ "Media confianza",
      pb1_01 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_02 = case_when(
      pb1_02 %in% 0:3 ~ "Baja confianza",
      pb1_02 %in% 4:7 ~ "Media confianza",
      pb1_02 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_03 = case_when(
      pb1_03 %in% 0:3 ~ "Baja confianza",
      pb1_03 %in% 4:7 ~ "Media confianza",
      pb1_03 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_04 = case_when(
      pb1_04 %in% 0:3 ~ "Baja confianza",
      pb1_04 %in% 4:7 ~ "Media confianza",
      pb1_04 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_05 = case_when(
      pb1_05 %in% 0:3 ~ "Baja confianza",
      pb1_05 %in% 4:7 ~ "Media confianza",
      pb1_05 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_06 = case_when(
      pb1_06 %in% 0:3 ~ "Baja confianza",
      pb1_06 %in% 4:7 ~ "Media confianza",
      pb1_06 %in% 8:10 ~ "Alta confianza"
    ),
    conf_pb1_07 = case_when(
      pb1_07 %in% 0:3 ~ "Baja confianza",
      pb1_07 %in% 4:7 ~ "Media confianza",
      pb1_07 %in% 8:10 ~ "Alta confianza"
    )
  )

# Factores ordenados
base <- base %>%
  mutate(across(
    starts_with("satisf_"),
    ~ factor(.x, levels = c("Baja satisfacción", "Media satisfacción", "Alta satisfacción"))
  )) %>%
  mutate(across(
    starts_with("conf_"),
    ~ factor(.x, levels = c("Baja confianza", "Media confianza", "Alta confianza"))
  ))

# Borrar variables que no se usan
base <- base %>% 
  select(-c(pa1,
            pa2,
            pa3_01,
            pa3_02,
            pa3_03,
            pa3_04,
            pa3_05,
            pa3_06,
            pa3_07, 
            pb1_01,
            pb1_02,
            pb1_03,
            pb1_04,
            pb1_05,
            pb1_06,
            pb1_07))


# Renombrar variables
base <- base %>%
  rename(
    edo_civil = p4_9,
    satisf_vida_actual = satisf_pa1,
    satisf_vida_pasado = satisf_pa2,
    satisf_nivel_vida = satisf_pa3_01,
    satisf_salud = satisf_pa3_02,
    satisf_logros = satisf_pa3_03,
    satisf_relaciones = satisf_pa3_04,
    satisf_amistades = satisf_pa3_05,
    satisf_familia = satisf_pa3_06,
    satisf_pareja = satisf_pa3_07,
    conf_gente_general = conf_pb1_01,
    conf_gente_conocida = conf_pb1_02,
    conf_medios = conf_pb1_03,
    conf_policia_mpal = conf_pb1_04,
    conf_policia_estatal = conf_pb1_05,
    conf_guardia_nal = conf_pb1_06,
    conf_ejercito = conf_pb1_07)

# Explorar los datos ----

# Tablas de contingencia: 
#        útiles para verificar la consistencia
#        de los datos y posibles relaciones significativas

# Considerar:
# Categorías desbalanceadas (una categoría con más del 80% de los datos)
# Celdas sin observaciones
# Relaciones no signficativas

# Verifico si hay na's
na <- base %>% 
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nas") %>%
  mutate(porcentaje = round(nas/nrow(base) * 100, 2)) %>%
  arrange(desc(nas))

na

rm(na)

# Lista de variables para tablas de frecuencias
variables <- c("paren",
               "edo_civil",
               "pea_status",
               "gpo_edad", 
               "satisf_vida_actual",
               "satisf_vida_pasado",
               "satisf_nivel_vida", 
               "satisf_salud",
               "satisf_logros",
               "satisf_relaciones", 
               "satisf_amistades",
               "satisf_familia",
               "satisf_pareja", 
               "conf_gente_general",
               "conf_gente_conocida",
               "conf_medios", 
               "conf_policia_mpal",
               "conf_policia_estatal",
               "conf_guardia_nal", 
               "conf_ejercito")

# Tablas de frecuencia: verificar categorías
tablas_frecuencia <- map(variables, ~{
  base %>% 
    tabyl(!!sym(.x)) %>% 
    adorn_pct_formatting()
}) %>%
  set_names(variables)

tablas_frecuencia

# Recodificar algunas categorías
base <- base %>%
  mutate(
    paren = case_when(
      paren %in% c("Jefa(a)", "Esposo(a)", "Hijo(a)") ~ as.character(paren),
      paren %in% c("Nieto(a)", "Yerno/Nuera", "Padre,Madre, Suegro(a)", 
                   "Otro parentesco", "Sin parentesco") ~ "Otros",
      TRUE ~ as.character(paren)),
    paren = factor(paren, levels = c("Jefa(a)", "Esposo(a)", "Hijo(a)", "Otros")))


base <- base %>%
  mutate(
    edo_civil = case_when(
      edo_civil %in% c("Unión libre", "Matrimonio", "Soltería") ~ as.character(edo_civil),
      edo_civil %in% c("Separación", "Divorcio", "Viudez", "No sabe") ~ "Otros",
      TRUE ~ as.character(edo_civil)),
    edo_civil = factor(edo_civil, levels = c("Unión libre", "Matrimonio", "Soltería", "Otros")))

# Vericar
tablas_frecuencia <- map(variables, ~{
  base %>% 
    tabyl(!!sym(.x)) %>% 
    adorn_pct_formatting()
}) %>%
  set_names(variables)

tablas_frecuencia

# Probar significancia de relaciones (ejemplo) 
tabla1<-table(base$nivel, base$satisf_vida_actual)
tabla1<-addmargins(tabla1)
tabla1
chisq.test(tabla1)

tabla2<-table(base$nivel, base$conf_medios)
tabla2<-addmargins(tabla2)
tabla2
chisq.test(tabla2)

tabla3<-table(base$pea_status, base$satisf_vida_actual)
tabla3<-addmargins(tabla3)
tabla3
chisq.test(tabla3)

# Realizar ACM FINAL ----

# Crear un objeto con las variables para el ACM
basecor <- base %>% select(sexo, gpo_edad, pea_status, nivel,satisf_vida_actual,conf_medios)

names(basecor)

# Para utilizar variables que no se usen en
# el cálculo pero que aparezcan en los ejes:
#       quanti.sup =
#       quanli.sup =

# Análisis
acm <- MCA(basecor, graph = TRUE, ncp = 14) #ncp especifica número de dimensiones a guardar

# Normalmente se omite el gráfico (se realiza después).
# Si no se omite: arroja 3 visualizaciones
#
# 1. Mapa de categorías
#
# 2. Mapa de individuos 
#
# 3. Representación de variables: 
#               qué tanto explica cada dimensión cada una variable de las variables que utilizamos. 
#               Si una variable está cerca del origen: puede que no aporte el ACM
#               Mejor aporte = cruce de ejes, lejos del origen
#               Aporte moderado: cercanía a un eje pero lejos del origen

# Utilizar 2 o 3 dimensiones es lo más común
#    se recomienda hacerlo si ya se conocen los datos
#    y la teoría respalda utilizar una representación bi/tridimensional

# Si no se conocen los datos: 
#    hacer el análisis completo para decidir cuántas dimensiones utilizar

## Resultados del análisis ----
# El comando MCA genera un objeto que contiene lo siguiente:
# COMPONENTES PRINCIPALES:
# 1.  $eig            - Eigenvalues: es la varianza explicada por cada dimensión.
#                       Se usa para decidir cuántas dimensiones conservar
#
# VARIABLES Y CATEGORÍAS ($var):
# 2.  $var$coord      - Coordenadas de categorías en el mapa (posición X,Y)
# 3.  $var$cos2       - Calidad de representación de categorías (0-1)
#                       Valores altos = bien representadas en el mapa
# 4.  $var$contrib    - Contribución de categorías a dimensiones (%)
#                       Se usa para identificar las categorías más importantes para cada eje
# 5.  $var$v.test     - Significancia estadística de posición de categorías
# 6.  $var$eta2       - Correlación variables-dimensiones (0-1)
#                       Indica qué variables definen cada dimensión
#
# INDIVIDUOS ($ind):
# 7.  $ind$coord      - Son las coordenadas de individuos en el mapa
# 8.  $ind$cos2       - Indica la calidad de representación de los individuos (0-1)
# 9.  $ind$contrib    - Indica la contribución de los individuos a las dimensiones
#
# OTRA TÉCNICA ($call):
# 10. $call           - Parámetros y resultados intermedios
# 11. $call$marge.col - Pesos/frecuencias de categorías
# 12. $call$marge.li  - Pesos de individuos
#
# PARA INTERPRETAR SE USAN:
# - acm$eig           (varianza explicada)
# - acm$var$coord     (coordenadas para mapas)
# - acm$var$contrib   (importancia de categorías)
# - acm$var$eta2      (qué variables definen cada dimensión)

# Interpretar dimensiones ----

# Obtener resumen del análisis

summary(acm)

# Explorar acm (todas las dimensiones)
# Se puede ver lo siguiente:
# 
#     1. Eigenvalues: cuánta varianza captura cada dimensión
#                     Dim1 Explica el 11.43% de las diferencias
#                     Dim1+Dim2+Dim3 explican el 28.3%
#                     Cuando la varianza acumulada deja de incrementarse (p.ej. de Dim4 a Dim5), podríamos dejar de considerar las siguientes dimensiones. Aunque de forma práctica lo usual es utiliza de 2 a 3 dimensiones nada más (ver correción de eigenvalues)
# 
#     2. Valores individuales
#                     Coordenadas de cada individuo en el mapa
#                     cos2: calidad de la representación (de 0 a 1),
#                           indica en qué dim está mejor representado un valor
#                     
#     3. Valores de las categorías:
#                     Cómo contribuyen las categorías a cada dimensión
#                     La contribución se lee positivamente (hacia la derecha) o negativamente (hacia la izquierda)
#                     cos2
#     
#     4. Correlación entre dimensiones y categorías
#                     Cómo se relaciona cada variable con cada dimensión
                    
## ¿Eigenvalues aceptables? ----
#
# Un ACM utiliza múltiples variables categóricas para
# calcular las distancias, por lo que la varianza explicada
# puede parecer menor a otros análisis.
# A esto se suma que los datos sociales implican
# mayor complejidad en las relaciones, 
# lo que disminuye los valores de la varianza explicada.
#                    
# Para evaluar mejor la varianza explicada se pueden utilizar correciones

# Correción de resultados ----

# Es recomendable corregir los resultados del ACM
# pues el análisis posee un sesgo: subestima las 
# distancias entre categorías y sobreestima el 
# número de dimensiones necesarias
#
# EFECTO DILUCIÓN
# Las variables categóricas crean muchas
# columnas binarias (0/1) en la matriz de datos
#
# INERCIA ARTIFICIAL
# Se genera inercia que no refleja las 
# verdaderas asociaciones entre variables
#
# DIMENSIONES ESPURIAS
# Se generan más dimensiones de las realmente significativas

# Correción de Benzécri
# Ajusta los eigenvalues con la siguiente fórmula:
#
#       λ_ajustado = (K/(K-1))² × (λ_original - 1/K)²
#
#                     K = número de variables
#                     λ = valor propio (eigenvalue)
#
#    - Corrige los eigenvalues:
#             elimina la inercia artificial
#    - Reduce dimensiones:
#             identifica las dimensiones realmente importantes
#    - Mejora interpretación:
#             las distancias se vuelven más realistas
#    - Elimina ruido:
#             filtra variabilidad espuria

# Calculo manual:
K <- ncol(basecor)  # Para definir número de variables
eigenvalues_orig <- acm$eig[,1]
eigenvalues_corr <- ((K/(K-1))^2) * ((eigenvalues_orig - 1/K)^2)

# quito notación científica
options(scipen = 999)

# Comparar
data.frame(
  Dimension = 1:length(eigenvalues_orig),
  Original = eigenvalues_orig,
  Corregido = eigenvalues_corr,
  Var_Explicada_Orig = acm$eig[,2],
  Var_Explicada_Corr = (eigenvalues_corr/sum(eigenvalues_corr))*100)

## Resultados de la correción ----
# Identificar dimensiones importantes
indices_importantes <- which(eigenvalues_corr > 0.001)
cat("Dimensiones importantes:", indices_importantes, "\n")
indices_importantes

# Ver valores
dim_importantes <- data.frame(
  Dimension = indices_importantes,
  Eigenvalue_Original = eigenvalues_orig[indices_importantes],
  Eigenvalue_Corregido = eigenvalues_corr[indices_importantes],
  Var_Explicada_Corregida = (eigenvalues_corr[indices_importantes]/sum(eigenvalues_corr))*100
)

print(dim_importantes)

# Como resultado: es mejor utilizar las dimensiones 1, 14, 13, 2, 12 (orden de importancia)

# Ver variables relacionadas con las dimensiones después de la correción
acm$var$eta2[, c(1, 2, 12, 13, 14)]

# Ver la contribución de las categorías en esas dimensiones
round(acm$var$contrib[, c(1, 2, 12, 13, 14)], 3)

# Interpretación después de la correción ----
#
# Es necesario analizar en conjunto cuatro dimensiones identificadas después de la corrección de Benzécri, con la varianza corregida:
#      -  Dimensión 1 (46.5%)
#      -  Dimensión 2 (6.3%)
#      -  Dimensión 12 (5.1%)
#      -  Dimensión 13 (10.6%) 
#      -  Dimensión 14 (27.6%)
#
# Estas cinco dimensiones explican el 96.1% de la variabilidad
#
# 1. Dim1 (46.5% varianza corregida): Eje socioeconómico principal
# 2. Dim2 (6.3% varianza corregida): Eje demográfico-educativo
# 3. Dim14 (27.6% varianza corregida): Eje socioeconómico secundario  
# 4. Dim13 (10.6% varianza corregida): Eje educacional-generacional
# 5. Dim12 (5.1% varianza corregida): Eje de satisfacción y confianza

# Dim1: Autonomía vs Dependencia socioeconómica
#   - Ocupados/jóvenes-adultos vs PNEA/adultos mayores
#
# Dim2: Diferenciación demográfica-educativa
#   - Hombres jóvenes con educación media vs mujeres adultas
#
# Dim14: Diferenciación laboral específica  
#   - Matices de situación laboral con componentes demográficos
#
# Dim13: Estratificación educativa
#   - Diferencias por nivel educativo y variación etaria
#
# Dim12: Bienestar subjetivo
#   - Satisfacción con la vida y confianza


# Contribuciones para las dimensiones después de la correción
fviz_contrib(acm, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribuciones Dimensión 1 (46.5% corregida)")

fviz_contrib(acm, choice = "var", axes = 2, top = 10) +
  labs(title = "Contribuciones Dimensión 2 (6.3% corregida)")

fviz_contrib(acm, choice = "var", axes = 14, top = 10) +
  labs(title = "Contribuciones Dimensión 14 (27.6% corregida)")

fviz_contrib(acm, choice = "var", axes = 13, top = 10) +
  labs(title = "Contribuciones Dimensión 13 (10.6% corregida)")

fviz_contrib(acm, choice = "var", axes = 12, top = 10) +
  labs(title = "Contribuciones Dimensión 12 (5.1% corregida)")


# Cos2 para los planos importantes según Benzécri
fviz_cos2(acm, choice = "var", axes = c(1, 14)) +
  labs(title = "Calidad representación: Dim 1 vs 14 (74.1% varianza corregida)")

fviz_cos2(acm, choice = "var", axes = c(13, 2, 12)) +
  labs(title = "Calidad representación: Dim 13 vs 2 vs 12 (22% varianza corregida)")

# Mapas ----

## Representación de variables ----
fviz_mca_var(acm, 
             choice = "mca.cor",   # Coordenadas de categorías
             repel = TRUE)

## Elaborar mapa factorial de las categorías ----
fviz_mca_var(acm, repel = TRUE)


### Colorear por contribución de variable -----
fviz_mca_var(acm, 
             col.var = "contrib",
             repel = TRUE)

# Considerar sólo las categorías que más contribuyen 
fviz_mca_var(acm, 
             select.var = list(contrib = 10),
             col.var = "contrib",
             repel = TRUE)

### Colorear por calidad de representación ----
fviz_mca_var(acm, 
             col.var = "cos2",
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

# Considerar mejor representación
fviz_mca_var(acm, 
             select.var = list(cos2 = 0.4), # cos2 > 0.4, se puede modificar
             col.var = "cos2",
             repel = TRUE)

### Graficar dimensiones específicas ----
# Por default da las Dim's 1 y 2, pero necesitamos las Dim's 1, 2, 12, 13 y 14

# Dim 1 y 14
fviz_mca_var(acm,
             axes = c(1, 14),
             col.var = "contrib", 
             repel = TRUE) +
  labs(title = "ACM Confianza y Satisfacción",
       subtitle = "Dimensiones 1 y 14",
       x = "Dim1 (46.5%)",
       y = "Dim14 (27.6%)")

# Dim 14 y 13
fviz_mca_var(acm, axes = c(14, 13), 
             col.var = "contrib", 
             repel = TRUE) +
  labs(title = "ACM Confianza y Satisfacción con la vida", 
       subtitle = "Dimensiones 14 y 13",
       x = "Dim14 (27.6%)",
       y = "Dim13 (10.6%)")

# Dim 2 y 12
fviz_mca_var(acm, axes = c(2, 12), 
             col.var = "contrib", 
             repel = TRUE) +
  labs(title = "ACM Confianza y Satisfacción con la vida", 
       subtitle = "Dimensiones 2 y 12",
       x = "Dim2 (6.3%)",
       y = "Dim12 (5.1%)")

### Cambiar formas, tamaño, etiquetas ----
fviz_mca_var(acm, 
             axes = c(1, 14),
             col.var = "cos2",
             shape.var = 17,         # Triángulo
             pointsize = "contrib",  # Tamaño por contribución
             labelsize = 3,          # Tamaño de etiquetas
             repel = TRUE)

# # Círculos y puntos
# shape.var = 1   # Círculo vacío
# shape.var = 16  # Círculo relleno
# shape.var = 19  # Círculo relleno (por defecto)
# shape.var = 20  # Punto pequeño relleno
# shape.var = 21  # Círculo con borde
# 
# # Cuadrados
# shape.var = 0   # Cuadrado vacío
# shape.var = 15  # Cuadrado relleno
# shape.var = 22  # Cuadrado con borde
# 
# # Triángulos
# shape.var = 2   # Triángulo vacío (hacia arriba)
# shape.var = 6   # Triángulo vacío (hacia abajo)
# shape.var = 17  # Triángulo relleno (hacia arriba)
# shape.var = 25  # Triángulo relleno (hacia abajo)
# shape.var = 24  # Triángulo con borde (hacia arriba)
# 
# # Diamantes
# shape.var = 5   # Diamante vacío
# shape.var = 18  # Diamante relleno
# shape.var = 23  # Diamante con borde
# 
# # Cruces y símbolos
# shape.var = 3   # Cruz (+)
# shape.var = 4   # Aspa (×)
# shape.var = 7   # Cuadrado con cruz
# shape.var = 8   # Estrella
# shape.var = 9   # Diamante con cruz
# shape.var = 10  # Círculo con cruz
# shape.var = 11  # Triángulo con cruz
# shape.var = 12  # Cuadrado con cruz
# shape.var = 13  # Círculo con aspa
# shape.var = 14  # Cuadrado con aspa

# fviz no reconoce grupos de variables/categorías para colorear o poner formas distintas a cada variable. Podría hacerse con ggplot2 extrayendo las coordenadas del resultado del ACM

## Mapa de individuos e individuos+categorías ----

# Tardan mucho con fviz(), depende de la cantidad de observaciones
# biplot dificulta lectura

# Mapa de individuos
# fviz_mca_ind(acm, axes = c(1, 14),)

# fviz_mca_biplot(acm, axes = c(1, 14), repel = TRUE)


# Sólo considerar la Dim1: "Autonomía vs Dependencia"

# Extraer coordenadas de Dim1 para individuos
coordenadas_dim1 <- acm$ind$coord[, 1]

# Índice normalizado
# Normalizar coordenadas (0 a 100)
indice <- ((coordenadas_dim1 - min(coordenadas_dim1)) /
                       (max(coordenadas_dim1) - min(coordenadas_dim1))) * 100

# Agregar índice a base
base$indice <- indice

# Categorías del índice
base <- base %>%
  mutate(
    indice_cat = case_when(
      indice >= 0 & indice < 33.33 ~ "Ocupados: jóvenes-adultos",
      indice >= 33.33 & indice < 66.67 ~ "Intermedios",
      indice >= 66.67 & indice <= 100 ~ "Desocupados adultos mayores/PNEA"
    ),
    indice_cat = factor(indice_cat,
                                 levels = c("Ocupados: jóvenes-adultos", "Intermedios", "Desocupados adultos mayores/PNEA"))
  )

# Revisar índice
summary(base$indice)

# Revisar índice categórico
base %>%
  tabyl(indice_cat) %>%
  adorn_pct_formatting()

# Validar índice con asociaciones a otras variables
# Teóricamente: Dim1 separa ocupados-jóvenes-adultos vs PNEA/adultos mayores

# Relación con pea
tabla_pea <- table(base$pea_status, base$indice_cat)
addmargins(tabla_pea)
chisq.test(tabla_pea)

# Relación con edad
tabla_edad <- table(base$gpo_edad, base$indice_cat)
addmargins(tabla_edad)
chisq.test(tabla_edad)

# Correlación con otras variables: satisfacción con la vida
cor.test(base$indice, as.numeric(base$satisf_vida_actual))

# Confirmar con correlación de spearman dado que satisfacción con la vida es categórico
cor.test(base$indice, as.numeric(base$satisf_vida_actual), method = "spearman")

# =========================================================
# Repaso chi-cuadrada
# Fecha: 20/05/2024
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "socviz", "RColorBrewer")

#Documentos socviz
#https://cran.r-project.org/web/packages/socviz/socviz.pdf

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



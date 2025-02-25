# =========================================================
# Intervalo de confianza para la proporción
# Fecha: 27/02/2025
# Autora: Mónica Lara Escalante-FLACSO México
# =========================================================


# Previo ------------------------------------------------------------------
pacman::p_load("tidyverse", "haven")

# Quitamos notación científica
options(scipen=999)


# Intervalo de confianza para la proporción (simulación) ------------------

# Creamos una población de 94 millones de personas donde el 60% aprueba el desempeño de la Presidenta.

N <- 94000000 #Población
p <- 0.6      #60% que aprueba (parámetro)

#Vamos a poner 1 si las personas aprueban y 0 si desaprueban
aprueban <- rep(1, p*N) # Crea vector de 1's representando a quienes aprueban
desaprueban <- rep(0,(1-p)*N) # Crea vector de 0's representando a quienes desaprueban
poblacion <- c(aprueban, desaprueban) #simulamos nuestra población

# Ahora calculamos un intervalo de confianza para p.

# Primero establecemos el nivel de confianza
confianza <- 0.95

# Luego calculamos nuestro valor de Z

#La función qnorm() devuelve el cuantil (o percentil) de una distribución normal estándar o de una normal con media y desviación estándar especificadas. 
#Es decir, dado un valor de probabilidadp, qnorm(p) encuentra el valor x tal que la probabilidad acumulada hasta xen una distribución normal es igual a p.

#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#p: Probabilidad acumulada hasta un valor x (debe estar entre 0 y 1).
#mean: Media de la distribución normal (por defecto es 0).
#sd: Desviación estándar de la distribución normal (por defecto es 1).
#lower.tail: Si es TRUE (por defecto), devuelve el cuantil asociado a P(X≤x)=p. 
#log.p: Si es TRUE, se asume que p está en escala logarítmica.

q <- qnorm((1-confianza)/2, lower.tail = F)

#(1-confianza)/2 porque:
#1-confianza = 0.05 (el nivel de significancia α)
#Queremos los puntos críticos que dejan α/2 en cada cola de la distribución
#Al ser bilateral y simétrico, dividimos α entre 2

#Definimos el tamaño de la muestra.
n <- 10

# Obtenemos la muestra aleatoria,
set.seed(123)
muestra <- sample(poblacion, size=n)

# Calculamos la estimación puntual,
p.hat <- sum(muestra)/n

# Estimamos la desviación estándar del estimador
sd.hat <- sqrt(p.hat*(1-p.hat)/n)

# Contruimos el intervalo
u <- p.hat + q*sd.hat
l <- p.hat - q*sd.hat
intervalo <- c(l,u)
intervalo

# Ahora simulamos el experimento de extraer cien muestras aleatorias y calcular 100 intervalos de confianza. Guardaremos los 100 intervalos en una tabla con el estimador puntual, el extremo izquierdo (l) y el extremo derecho (u).

n <- 10
confianza <- 0.95
q <- qnorm((1-confianza)/2, lower.tail=F)
intervalos <- tibble(p=c(),l=c(),u=c())

set.seed(123) # Número semilla

for(i in 1:100){ # Se ejecuta el proceso 100 veces
muestra <- sample(poblacion,size=n) # Toma una muestra aleatoria
p.hat <- sum(muestra)/n # Calcula la proporción muestral
sd.hat <- sqrt(p.hat*(1-p.hat)/n) # Calcula el error estándar
u <- p.hat + q*sd.hat # Límite superior del intervalo
l <- p.hat - q*sd.hat # Límite inferior del intervalo
intervalos <- rbind(intervalos, data.frame(p=p.hat,l=l,u=u)) #Almacena cada intervalo calculado en un data frame
}

intervalos

#¿Cuáles muestras tienen los valores menores de nuestro estimador? ¿Y los mayores?
#¿Cómo podríamos ver cuáles intervalos son más anchos?

# Agregamos una columna que nos diga si el intervalo contiene o no al verdadero valor p=0.6.

intervalos %>%
  mutate(
# Crea columna 'contiene_a_p' que verifica si el valor 0.6 está dentro del intervalo [l,u]
    contiene_a_p = ifelse(
      l <= 0.6 & 0.6 <= u,  
# Condición: ¿está 0.6 entre l y u (inclusive)?
      T,  # Si es verdadero, asignar TRUE
      F  # Si es falso, asignar FALSE
    )
  ) -> intervalos # Asignar el resultado de vuelta al dataframe 

#¿Cuántos intervalos contienen el verdadero valor de p?
sum(intervalos$contiene_a_p)/nrow(intervalos)

#Grafiquemos

intervalos %>% 
  ggplot(aes(x=1:100,y=p,col=contiene_a_p))+
  geom_point(size=4) +
  geom_errorbar(aes(ymax=u,ymin=l))+
  geom_line(aes(y=0.6),linetype="dotted")+
  scale_color_manual(values=c("red","black"))+
  ylim(c(-0.1,1.1))+
  theme_bw()+
  labs(
    title="Intervalo de confianza para la proporción de aprobación de la Presidenta",
    subtitle="A partir de datos simulados",
    caption="Elaboración propia",
    x="Muestra",
    y="Proporción estimada",
    color="¿Contiene a P?"  
  )


# Intervalo para la proporción (datos reales) -----------------------------

# Abrimos la base de datos de LAPOP México
mex_2023 <- read_dta("C:/Users/molar/Dropbox/2025_Trabajos/FLACSO/MCS/Datos/mex_2023.dta")

#Ahora, hablando de la gente de por aquí, ¿diría que la gente de su comunidad es muy confiable, algo confiable, poco confiable o nada confiable?

#Veamos la proporción de personas que consideran que la gente es muy confiable y calculemos el intervalo de confianza.
table(mex_2023$it1)
exitos <- 331
n <- 331+561+546+171

#prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE)

#x: Representa el número de éxitos o casos positivos observados. Puede ser un vector si estás comparando múltiples proporciones.
#n: Es el número total de ensayos o tamaño de la muestra. También puede ser un vector correspondiente a cada valor en x.
#p = NULL: Es la proporción hipotética bajo la hipótesis nula. Si no se especifica (NULL), la función asume que las proporciones son iguales en todos los grupos.
#alternative = c("two.sided", "less", "greater"): Especifica el tipo de hipótesis alternativa
#conf.level = 0.95: Define el nivel de confianza para el intervalo calculado (por defecto es 0.95, lo que corresponde a un 95% de confianza).
#correct = TRUE: Indica si se debe aplicar la corrección de continuidad de Yates, que es una ajuste para mejorar la aproximación de la distribución discreta binomial a la distribución continua chi-cuadrado.

prop.test(exitos, 
          n, 
          alternative=c("two.sided"),
          conf.level = 0.95,
          correct=F)

# Graficamos

# Datos del intervalo de confianza
p_estimado <- 0.2057178      # Proporción estimada
ic_inferior <- 0.1866786     # Límite inferior del IC al 95%
ic_superior <- 0.2261589     # Límite superior del IC al 95%

# Crear un dataframe para el gráfico
datos <- data.frame(
  etiqueta = "Proporción",
  estimacion = p_estimado,
  inferior = ic_inferior,
  superior = ic_superior
)

# Gráfico con orientación horizontal
ggplot(datos, aes(y = etiqueta, x = estimacion)) +
  # Punto para la estimación puntual
  geom_point(size = 3, color = "purple") +
  # Barra de error horizontal para el intervalo de confianza
  geom_errorbarh(aes(xmin = inferior, xmax = superior), height = 0.2, linewidth = 1, color = "magenta") +
  # Personalización del gráfico
  labs(
    title = "Intervalo de Confianza al 95% para la Proporción",
    y = "",
    x = "Proporción"
  ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )


# Ejercicio ---------------------------------------------------------------

# 1. Replicar el ejercicio de la simulación con 1000 muestras de tamaño 30
# 2. Replicar el ejercicio de LAPOP con otra variable.
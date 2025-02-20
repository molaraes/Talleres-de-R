#Ahora con muestras de tamaño 30
n <- 30
estimaciones.30 <- c()
set.seed(123)

for(i in 1:1000) {
  muestra<- sample(poblacion, size=n)
  muestra
  p_gorro <- sum(muestra)/length(muestra)
  estimaciones.30[i] <- p_gorro
}

estimaciones.30
min(estimaciones.30)
max(estimaciones.30)



#Ahora con muestras de tamaño 100
n <- 100
estimaciones.100 <- c()
set.seed(123)

for(i in 1:1000) {
  muestra<- sample(poblacion, size=n)
  muestra
  p_gorro <- sum(muestra)/length(muestra)
  estimaciones.100[i] <- p_gorro
}

estimaciones.100
min(estimaciones.100)
max(estimaciones.100)



#Vamos a juntar los tres grupos de estimaciones en una misma tabla
data <- tibble(estimaciones.10, estimaciones.30, estimaciones.100)

data <- data %>% 
  pivot_longer(estimaciones.10:estimaciones.100) #cambiamos de formato ancho a largo

data %>% #tome la base de datos
  ggplot(aes(x=value, fill=name))+ 
  geom_histogram(aes(y=..density..), binwidth = 0.05,color="black", position="identity", alpha=0.5)+
  scale_fill_manual(values=c("skyblue", "violet", "magenta"),name="Tamaño de muestra",
labels=c(10, 100, 30))+
  ggtitle("Distribución muestral de estimadores puntuales")+
  xlab("Estimación")+
  ylab("Frecuencia")+
  theme_classic()


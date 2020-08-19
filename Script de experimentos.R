#######Documentos de experimentos en R#####
##Jueves 02-04-2020

#######Inicio del código###########

rm(list = ls()) #borra objetos al iniciar un nuevo codigo
(.packages()) #Comando para ver paquetes cargados

####### Instalando bibliotecas 

install.packages("ggmap")

###### Cargando bibliotecas

library(foreign) #Contiene funciones para leer archivos de distintos formatos
library(dplyr)   #Facilita trabajo con bases de datos
library(psych)   #Funciones para calcular descriptivos
library(ggplot2) #Funciones para graficar
library (ggmap) #Funciones para llamar mapas, de Google Maps

###### Descriptivos por Base de datos$variables
summary(HSBdata$locus_of_control)
summary(HSBdata$self_concept)

sd_data_locus_control <- sd(HSBdata$locus_of_control)
sd_data_selfconcept <- sd(HSBdata$self_concept)

var_data_locus_control <- var(HSBdata$locus_of_control)  #Sin NAs
var_data_selfconcept <- var(HSBdata$self_concept, na.rm = T) #Con NAs

describe(HSBdata)

####### Graficando

locus_of_control <- HSBdata$locus_of_control
self_concept <- HSBdata$self_concept

locus_of_control
self_concept

plot(locus_of_control, self_concept)

qplot(locus_of_control, self_concept) 



grafico_data_locus_control <- ggplot(HSBdata, aes(locus_of_control)) +
  geom_histogram(fill = "lavender", colour = "black", binwidth = 20 ) + 
  theme_classic() +
  geom_vline(aes(xintercept = mean(locus_of_control)), 
             linetype = "dashed", size = 1, colour = "Aquamarine") +
  geom_vline(aes(xintercept = median(locus_of_control)),
             colour = "Plum", size = 1)

grafico_data_locus_control


######## Mapas

###### Guardar Base de datos en Archivo (debe existir)
data_a_guardar <- HSBdata
write.table(data_a_guardar,file="C:/Talleres_phd/R/Clase 1/table.txt",sep="\t",row.names=FALSE,col.names=TRUE)











## Tarea 1: Comparación de grupos usando estadística descriptiva
## EDU4045: Introducción a los métodos cuantitativos en educación
## Profesores: Ernesto Treviño y Rosario Escribano
##
## Estudiante: Pablo Barrientos Saavedra
##
#########1. Inicio del código #####
rm(list = ls())
(.packages()) 
install.packages("nortest")

library(dplyr)
library(psych)   
library(ggplot2) 
library(tidyverse)
library(nortest)

#####PASO 1: Lectura de base de datos y selección de variables#####
base_simce <- readRDS("C:/Talleres_phd/R/Bases de datos/SIMCE/df_simce_est.rds")

head(base_simce)

##Selección de variables para trabajar
base_simce_redu <- base_simce %>% 
  select(id_i, id_j, id_k, plec, pmat, psci, clec, cmat, csci, gnro) 

base_simce_mat <- base_simce %>% 
  select(id_i, id_j, id_k, pmat, cmat, gnro) 

base_simce_sci <- base_simce %>% 
  select(id_i, id_j, id_k, psci, csci, gnro) 

## NA::::: Se podría usar na.omit() 

base_simce_mat$gnro <- na_if(base_simce_redu$gnro, 0)
base_simce_mat$gnro <- na_if(base_simce_redu$gnro, 99)

data_mat_sinNA <- base_simce_mat %>% filter(gnro != is.na(gnro))

table(data_mat_sinNA$gnro)

base_simce_redu <- base_simce_redu[complete.cases(base_simce_redu$plec),]
data_lec_sinNA <- base_simce_redu %>% filter(gnro != is.na(gnro))

typeof(base_simce_redu$id_i)

#DEFINIR NAs en base a trabajar
base_simce_sci <- na_if(base_simce_sci[,1:6], 0)
base_simce_sci <- na_if(base_simce_sci[,1:6], 99)
base_simce_sci <- na_if(base_simce_sci[,1:6], "")

base_simce_redu <- na_if(base_simce_redu[,4:10], 0)
base_simce_redu <- na_if(base_simce_redu[,1:7], 99)
base_simce_redu <- na_if(base_simce_redu[,1:7], "")

typeof(base_simce_redu$gnro)

######Crear bases por género y con casos completos#### 
base_simce_sci <- base_simce_sci[complete.cases(base_simce_sci$gnro),]

#Crear base hombre y mujer
#Cambiar tipo de variable "gnro" de valor numerico a entero.

#####Bases de Matemáticas por género#####

mates_hombre <- data_mat_sinNA %>% filter( gnro == 1)
mates_mujer <- data_mat_sinNA %>% filter( gnro == 2)


data_mat_sinNA$gnro <- as.factor(data_mat_sinNA$gnro)
mates_hombre$gnro <- as.factor(mates_hombre$gnro)
mates_mujer$gnro <- as.factor(mates_mujer$gnro)
typeof(mates_hombre$gnro)
typeof(mates_mujer$gnro)

#####Bases de Ciencias por género#####
ciencias_hombre <- base_simce_sci %>% filter(gnro == 1)
ciencias_mujer <- base_simce_sci %>% filter(gnro == 2)

ciencias_hombre$gnro <- as.factor(ciencias_hombre$gnro)
ciencias_mujer$gnro <- as.factor(ciencias_mujer$gnro)
typeof(ciencias_mujer$gnro)

base_simce_sci$gnro <- as.factor(base_simce_sci$gnro)

########################## Descriptivos por prueba y género######

descript_gnro <- base_simce_redu %>%  #Paso 1: defino la base de datos
  group_by(gnro) %>%  #Paso 2: defino la variable que agrupa
  summarise(media_leng = mean(plec, na.rm = T), #Paso 3: defino descriptivos
            mediana_leng = median(plec, na.rm = T),
            sd_leng = sd(plec, na.rm = T),
            max_leng = max(plec, na.rm = T),
            min_leng = min(plec, na.rm = T),
            rango_leng = max(plec) - min(plec),
            
            media_mat = mean(pmat, na.rm = T), #Paso 3: defino descriptivos
            mediana_mat = median(pmat, na.rm = T),
            sd_mat = sd(pmat, na.rm = T),
            max_mat = max(pmat, na.rm = T),
            min_mat = min(pmat, na.rm = T),
            rango_mat = max(pmat, na.rm = T) - min(pmat, na.rm = T),
            
            media_sci = mean(psci, na.rm = T), #Paso 3: defino descriptivos
            mediana_sci = median(psci, na.rm = T),
            sd_sci = sd(psci, na.rm = T),
            max_sci = max(psci, na.rm = T),
            min_sci = min(psci, na.rm = T),
            rango_sci = max(psci, na.rm = T) - min(psci, na.rm = T),
            
            N = n(), #Este cuenta los casos en cada categoria
            porcentaje = (n()/181136)*100) 

ungroup(base_simce_sci)

describe(data_mat_sinNA[,1:4])
describe(mates_hombre[,1:4])
describe(mates_mujer[,1:4])


#######PRUEBAS DE HIPÓTESIS#######
#######TEST DE NORMALIDAD (PARA MUESTRAS GRANDES)###########
ks.test(ciencias_hombre$psci, pnorm, mean(ciencias_hombre$psci, na.rm = T), sd(ciencias_hombre$psci, na.rm = T))

lillie.test(ciencias_hombre$psci)

lillie.test(ciencias_mujer$psci)

pearson.test(ciencias_hombre$psci)

?lillie.test()

##TEST DE HOMOCEDASTICIDAD, IGUALDAD DE VARIANZAS##############
bartlett.test(psci~gnro, base_simce_sci) 

########Test de igualdad de medias#########
t.test(base_simce_sci$psci, mu=mean(base_simce_sci$psci, na.rm = T))

t.test(base_simce_sci$psci)

#####Graficos de las curvas#####
###Gráfico de MATEMATICAS####
mate_plot <-  ggplot(data_mat_sinNA, aes(pmat, fill = gnro)) +
  geom_density(na.rm = T, position = "identity", alpha = 0.3) + 
  labs(title = "Distribución de puntajes de matemáticas por género") +
  labs(x = "Puntaje", y = "Densidad") + 
#  scale_x_continuous(breaks = seq(100, 400, 8)) +
  geom_vline(aes(xintercept = mean(mates_hombre$pmat, na.rm = T)),
             colour = "red", size=1) +
  geom_vline(aes(xintercept = mean(mates_mujer$pmat, na.rm = T)),
             colour = "blue", size=1) +
  theme_light() 

mate_plot

###Gráfico de CIENCIAS####
ciencia_plot <-  ggplot(base_simce_sci, aes(psci, fill = gnro)) +
  geom_density(na.rm = T, position = "identity", alpha = 0.3) + 
  labs(title = "Distribución de puntajes de ciencias por género") +
  labs(x = "Puntaje", y = "Densidad") + 
  xlim(99, 399) +
  geom_vline(aes(xintercept = mean(ciencias_hombre$psci, na.rm = T)),
             colour = "red", size=1) +
  geom_vline(aes(xintercept = mean(ciencias_mujer$psci, na.rm = T)),
             colour = "blue", size=1) +
  theme_light() 

ciencia_plot


#1:hombre, 2:mujer

### Test de normalidad ##

ks.test(simce_hombre$pmat, pnorm, mean(simce_hombre$pmat, na.rm = T), sd(simce_hombre$pmat, na.rm = T))
lillie.test(simce_hombre$pmat)

#################Continuación Tarea 1######

## Tipo de variable
base_simce_redu$id_i <- as.factor(base_simce_redu$id_i)
base_simce_redu$id_j <- as.factor(base_simce_redu$id_j)
base_simce_redu$id_k <- as.factor(base_simce_redu$id_k)
base_simce_redu$gnro <- as.factor(base_simce_redu$gnro)

#2. Estadística descriptiva (de un puntaje)

summary(base_simce_redu$plec)
describe(base_simce_redu$plec)

summary(data_mat_sinNA$pmat)
describe(data_mat_sinNA$pmat)

#a. Media = 245
#b. Mediana = 247.8
#c. Desviación estándar = 49.69 
#d. Máximo = 373.16
#e. Mínimo = 116.65
#f. Rango = 256.51
#g. Número de casos completos (plec) = 181136

##Objetos para el gráfico
plec_mean <- mean(base_simce_redu$plec, na.rm = T)
plec_sd <- sd(base_simce_redu$plec, na.rm = T)
plec_median <- median(base_simce_redu$plec, na.rm = T)

plec_mean
plec_sd
plec_median

typeof(plec_median)
typeof(plec_mean)

sup1_plec <- plec_mean + plec_sd
sup2_plec <- plec_mean + 2*plec_sd
inf1_plec <- plec_mean - plec_sd
inf2_plec <- plec_mean - 2*plec_sd

## Grafique la distribución de puntaje, indicando en el gráfico 
## la media, mediana y limites superiores e inferiores para 1 y 2 desviaciones estándar
##HISTOGRAMA
histogram_simce_plec <- ggplot(base_simce_redu, aes(plec, na.rm = T)) +
  geom_histogram(fill = "blue", alpha = 0.3, colour = "steelblue4", binwidth = 2) + 
  labs (title = "Distribución nacional de puntajes Simce Lenguaje") +
  labs(x = "Puntaje", y = "N° de estudiantes") +
  xlim (100, 400) + 
  ylim (0, 3000) +
  theme_classic() + 
  geom_vline(aes(xintercept = plec_mean), 
             linetype = "solid", alpha = 0.9, size = 1, colour = "Orange") + 
  geom_vline(aes(xintercept = plec_median), 
             linetype = "dotdash", size = 1, colour = "tan3") + 
  geom_vline(aes(xintercept = sup1_plec), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = inf1_plec), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = sup2_plec), 
             linetype = "dashed", alpha = 0.9, size = 1, colour = "darkblue") + 
  geom_vline(aes(xintercept = inf2_plec), 
             linetype = "dashed", alpha = 0.9, size = 1, colour = "darkblue")
  
histogram_simce_plec

##4. Logro académico por género (opción 1: crear 2 bases)
simce_plec_niños <- base_simce_redu %>% filter(gnro == 1) 
simce_plec_niñas <- base_simce_redu %>% filter(gnro == 2) 

describe(simce_plec_niñas$plec)
describe(simce_plec_niños$plec)







##Graficos por género
histogram_lec_niños <- ggplot(simce_plec_niños, aes(plec)) +
  geom_histogram(fill = "grey", alpha = 0.3, colour = "grey44", binwidth = 3) + 
  labs (title = "Distribución puntajes Simce Lenguaje en niños") +
  labs(x = "Puntaje", y = "N° de estudiantes") + 
  xlim(100, 400) +
  theme_classic() + 
  geom_vline(aes(xintercept = mean(plec)), 
             linetype = "solid", alpha = 0.7, size = 1, colour = "Orange") + 
  geom_vline(aes(xintercept = median(plec)), 
             linetype = "dotdash", alpha = 0.7, size = 1, colour = "purple") + 
  geom_vline(aes(xintercept = mean(plec) + sd(plec)), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = mean(plec) - sd(plec)), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = mean(plec) + 2*sd(plec)), 
             linetype = "dashed", alpha = 0.7, size = 1, colour = "darkblue") + 
  geom_vline(aes(xintercept = mean(plec) - 2*sd(plec)), 
             linetype = "dashed", alpha = 0.7, size = 1, colour = "darkblue")

histogram_lec_niños

histogram_lec_niñas <- ggplot(simce_plec_niñas, aes(plec)) +
  geom_histogram(fill = "green", alpha = 0.3, colour = "green4", binwidth = 3) + 
  labs (title = "Distribución puntajes Simce Lenguaje en niñas") +
  labs(x = "Puntaje", y = "N° de estudiantes") + 
  xlim(100, 400) +
  geom_vline(aes(xintercept = mean(plec)), 
             linetype = "solid", alpha = 0.8, size = 1, colour = "Orange") + 
  geom_vline(aes(xintercept = median(plec)), 
             linetype = "dotdash", size = 1, colour = "Purple") + 
  geom_vline(aes(xintercept = mean(plec) + sd(plec)), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = mean(plec) - sd(plec)), 
             linetype = "dashed", size = 1, colour = "blue") + 
  geom_vline(aes(xintercept = mean(plec) + 2*sd(plec)), 
             linetype = "dashed", alpha = 0.9, size = 1, colour = "darkblue") + 
  geom_vline(aes(xintercept = mean(plec) - 2*sd(plec)), 
             linetype = "dashed", alpha = 0.9, size = 1, colour = "darkblue")

histogram_lec_niñas


muestra_niñas1 <- sample(simce_plec_niñas$plec, size=5000, replace=F)
muestra_niños1 <- sample(simce_plec_niños$plec, size=5000, replace=F)

describe(muestra_niños1)
describe(simce_plec_niños$plec)
shapiro.test(muestra_niños1)

shapiro.test(muestra_niñas1)

t.test(plec~gnro, base_simce_redu) ## Esta funcion requiere un argumento que es una funcion: plec ~ gnro, en la base SIMCE reducida)
##Otros argumentos, var.equal = T // le digo que sí se que las varianzas son iguales // , mu=(238 - 251) // agregamos la distancia de medias
## Prueba de 2 lados: alternative = "less", "rather" o "paired"
## != "Distinto a" filter(gnro == 1)

describe(muestra_niñas1)
shapiro.test(muestra_niñas1)

shapiro.test(simce_plec_niñas$plec)
shapiro.test(simce_plec_niños$plec)


##Descriptivos por género (usando group_by)
descript_gnro <- base_simce_redu %>%  #Paso 1: defino la base de datos
  group_by(gnro) %>%  #Paso 2: defino la variable que agrupa
  summarise(media = mean(plec, na.rm = T), #Paso 3: defino descriptivos
            mediana = median(plec, na.rm = T),
            des = sd(plec, na.rm = T),
            max = max(plec, na.rm = T),
            min = min(plec, na.rm = T),
            rango = max(plec) - min(plec),
            N = n(), #Este cuenta los casos en cada categoria
            porcentaje = (n()/181136)*100) 

##Graficar por nivel de desempeño
descript_desempeño <- base_simce_redu %>%
  group_by(clec) %>%  
  summarise(media = mean(plec, na.rm = T), 
            mediana = median(plec, na.rm = T),
            des = sd(plec, na.rm = T),
            max = max(plec, na.rm = T),
            min = min(plec, na.rm = T),
            rango = max(plec) - min(plec),
            N = n(), 
            porcentaje = (n()/181136)*100)

  
descript_desempeño %>% ggplot(aes(x=clec, y = media, fill = clec)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_light()+
  labs(x = "Nivel de desempeño", y = "Puntaje",
       title = paste("Puntaje promedio en SIMCE Lenguaje por nivel de desempeño"))

ungroup(base_simce_redu)

##Descriptivos por nivel de desempeño y género
descript_desemp_ninas <- simce_plec_niñas %>%  #Paso 1: defino la base de datos
  group_by(clec) %>%  #Paso 2: defino la variable que agrupa
  summarise(media = mean(plec, na.rm = T), #Paso 3: defino descriptivos
            mediana = median(plec, na.rm = T),
            des = sd(plec, na.rm = T),
            max = max(plec, na.rm = T),
            min = min(plec, na.rm = T),
            rango = max(plec) - min(plec),
            N = n(), #Este cuenta los casos en cada categoria
            porcentaje = (n()/87918)*100) 

view(descript_desemp_ninas)

descript_desemp_ninos <- simce_plec_niños %>%  #Paso 1: defino la base de datos
  group_by(clec) %>%  #Paso 2: defino la variable que agrupa
  summarise(media = mean(plec, na.rm = T), #Paso 3: defino descriptivos
            mediana = median(plec, na.rm = T),
            des = sd(plec, na.rm = T),
            max = max(plec, na.rm = T),
            min = min(plec, na.rm = T),
            rango = max(plec) - min(plec),
            N = n(), #Este cuenta los casos en cada categoria
            porcentaje = (n()/88558)*100) 

view(descript_desemp_ninos)

ungroup(simce_plec_niñas)
ungroup(simce_plec_niños)

##Tabla de frecuencia de puntajes por género y grupos de desempeño
#Nivel adecuado, N(niñas) = 18412, N(niños) = 14412 / N(total) = 32824
#Nivel elemental, N(niñas) = 32497, N(niños) = 27347 / N(total) = 61496 
#Nivel insuficiente, N(niñas) = 37009, N(niños) = 46799 / N(total) = 85910
adec_niñas <- 18412
adec_niños <- 14412
elem_niñas <- 32497 
elem_niños <- 27347
insuf_niñas <- 37009 
insuf_niños <- 46799 

P_adec_niñas <- (adec_niñas/(adec_niñas+adec_niños))*100
P_adec_niños <- (adec_niños/(adec_niñas+adec_niños))*100
P_elem_niñas <- (elem_niñas/(elem_niñas+elem_niños))*100
P_elem_niños <- (elem_niños/(elem_niñas+elem_niños))*100
P_insuf_niñas <- (insuf_niñas/(insuf_niñas+insuf_niños))*100 
p_insuf_niños <- (insuf_niños/(insuf_niñas+insuf_niños))*100

niveles <- c(adec_niñas, adec_niños, elem_niñas, elem_niños, insuf_niñas, insuf_niños)
porcentajes <- c(P_adec_niñas, P_adec_niños, P_elem_niñas, P_elem_niños, P_insuf_niñas, p_insuf_niños)
tabla_niveles <- data.frame(niveles, porcentajes)
##Graficar por género y grupos de desempeño

genero_desemp <- c(rep("Niñas" , 3) , rep("Niños" , 3))
nivel_desemp <- rep(c("Adecuado" , "Elemental" , "Insuficiente") , 2)
media_desemp <- c(315,268,204,314,267,199)
data.gnro.desemp <- data.frame(nivel_desemp,genero_desemp,media_desemp)

grafico_final <- ggplot(data.gnro.desemp, aes(fill=genero_desemp, y=media_desemp, x=nivel_desemp)) + 
  geom_bar(position="dodge", stat="identity", width = 0.5) +
  theme_light() +
  coord_cartesian(ylim = c(180,320)) +
  ggtitle("Puntaje promedio lenguaje", subtitle = "por nivel de desempeño y género") +
  xlab("Nivel de desempeño") +
  ylab("Puntaje promedio")

grafico_final

## Para la prueba t student (comparación de medias muestrales) La variable debe ser continua u ordinal
## los datos deben venir de muestras aleatorias simples de la población de interés
## Sirve para comparar 2 grupos (hombres y mujeres en SIMCE, p. e.)
## la varianza de los grupos debe ser homogenea 
## Si los datos son muy dispersos no sirve-
## SUPUESTOS DE LA PRUEBA T
## Homogeneidad entre las varianzas (estadísticamente similares)
## Distribución normal (obtenida aleatoriamente, p. e.)
## 

t.test(plec~gnro, base_simce_redu )

## p-value > 0,05 = rechaza la hipótesis nula.
## p-value < 0,05 = se acepta la hipótesis nula.
## H0 = ambas curvas siguen distribución normal y no hay diferencia
## significativa entre sus medias.
##  
## Cuando no se cumple alguno de los supuestos de la prueba t
## Se recurre a variaciones del test
## 
##
##
## shapiro.test(base_simce_redu$plec)
## El test de Shapiro p > 0,05 no puedo rechazar la H0
##
## barlett.test( plec~gnro, base_simce_redu)
## barlett.test se usa para complementar estas pruebas
## Si no se puede rechazar la H0, no significa que la apruebo.
## No existe evidencia para rechazar la Hipótesis Nula (H0)
## T.test / Shapiro / Barlett // Son tests de normalidad: para determinar si la distrubución es normal o no.
## Jugar T.test  

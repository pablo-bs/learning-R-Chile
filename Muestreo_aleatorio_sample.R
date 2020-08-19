#Uso de Sample_n para seleccionar aleatoriamente X número de filas de una tabla

library(dplyr)

base_simce <- readRDS("C:/Talleres_phd/R/Bases de datos/SIMCE/df_simce_est.rds")

simce_redu <- base_simce %>% 
  select(id_i, id_k, plec, pmat, clec, cmat, gnro,
         disc04, safe01) 

muestra1 <- sample_n(simce_redu, size = 4990, replace = F)

#Test de normalidad
shapiro.test(muestra1$pmat)
ks.test(muestra1$pmat, pnorm)
hist(muestra1$pmat)
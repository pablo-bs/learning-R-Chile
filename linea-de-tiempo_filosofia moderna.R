######INICIO DEL CODIGO ##### 
rm(list = ls())
(.packages()) 
##install.packages("vistime")

library(vistime)
library(ggplot2)

##### EJEMPLO 1: LINEA DE TIEMPO #####
pres <- data.frame(Position = rep(c("President", "Vice"), each = 3),
                   Name = c("Washington", rep(c("Adams", "Jefferson"), 2), "Burr"),
                   start = c("1789-03-29", "1797-02-03", "1801-02-03"),
                   end = c("1797-02-03", "1801-02-03", "1809-02-03"),
                   color = c('#cbb69d', '#603913', '#c69c6e'),
                   fontcolor = c("black", "white", "black"))

vistime(pres, events="Position", groups="Name", title="Presidents of the USA")

###### linea de tiempo FILOSOFÍA MODERNA ######

filosofia.moderna <- data.frame(Nombres = c("Nicolás Copernico", "Galileo", "Erasmo de Rotterdam", 
                                         "Nicolás Maquiavelo", "Etienne De la Boetie", "Michel de Montainge", "René Descartes", 
                                         "Thomas Hobbes", "David Hume", "Immanuel Kant", "John Locke", "Jean-Jacques Rousseau", 
                                         "François Poullain de la Barre","Friedrich Schiller", "Mary Wollstonecraft"),
                                Año_nacimiento = c("1473-02-19","1564-02-15","1466-10-28","1469-05-03","1530-11-01","1533-02-28","1596-03-31",
                                                   "1588-04-05","1711-05-07","1724-04-22","1632-08-29","1712-06-28",
                                                   "1647-07-01","1759-11-10","1759-04-27"),
                                Año_muerte     = c("1543-05-24","1642-01-08","1536-07-12","1527-06-21","1563-08-18","1592-09-13","1650-02-11",
                                                   "1679-12-04","1776-08-25","1804-02-12","1704-10-28","1778-07-02",
                                                   "1723-05-04","1805-05-09","1797-09-10")
                                                  )
filosofia.moderna1 <- data.frame(Nombres = c("Nicolás Copernico", "Galileo Galilei", "Erasmo de Rotterdam", 
                                            "Nicolás Maquiavelo", "Etienne De la Boetie", "Michel de Montainge", 
                                            "René Descartes", "Thomas Hobbes", "John Locke", "François Poullain de la Barre"),
                                 
                                Año_nacimiento = c("1473-02-19","1564-02-15","1466-10-28",
                                                   "1469-05-03","1530-11-01","1533-02-28",
                                                   "1596-03-31","1588-04-05","1632-08-29","1647-07-01"),
                                
                                Año_muerte     = c("1543-05-24","1642-01-08","1536-07-12",
                                                   "1527-06-21","1563-08-18","1592-09-13",
                                                   "1650-02-11","1679-12-04","1704-10-28", "1723-05-04") )

filosofia.moderna2 <- data.frame(Nombres = c("René Descartes", "Thomas Hobbes", "John Locke", 
                                             "David Hume", "Immanuel Kant", "François Poullain de la Barre",
                                             "Jean-Jacques Rousseau", "Friedrich Schiller", "Mary Wollstonecraft"),
                                 
                                 Año_nacimiento = c("1596-03-31","1588-04-05","1632-08-29",
                                                    "1711-05-07","1724-04-22", "1647-07-01",
                                                    "1712-06-28","1759-11-10","1759-04-27"),
                                 
                                 Año_muerte     = c("1650-02-11","1679-12-04","1704-10-28",
                                                    "1776-08-25","1804-02-12","1723-05-04",
                                                    "1778-07-02","1805-05-09","1797-09-10")
                                                    
                                                    )


##Interactive time-line
linea1 <- vistime(filosofia.moderna1, start = "Año_nacimiento", end = "Año_muerte", events = "Nombres", 
        title="Línea de tiempo filosofía moderna (1ra parte), s. XV - s. XVIII",
        optimize_y = FALSE, background_lines = 10)
  
linea1  

linea2 <- vistime(filosofia.moderna2 , start = "Año_nacimiento", end = "Año_muerte", events = "Nombres", 
                  title="Línea de tiempo filosofía moderna (2da parte), s. XVII - s. XVIII",
                  optimize_y = FALSE, background_lines = 10)

linea2 

?gg_vistime()

##Static time-line
gg_vistime(filosofia.moderna, start = "Año_nacimiento", end = "Año_muerte", events = "Nombres", 
                    title="Línea de tiempo, filosofía moderna, s. XV a XVIII",
                    linewidth = 6, optimize_y = FALSE, background_lines = 20)


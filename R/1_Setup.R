#* 
#* COMPLETACION DE DATOS DE PRECIPITACIONES DIARIAS
#* PLATAFORMA: ANDREA - ANA
#* 
#* Completacion de datos hidrologicos
#* @autor: Kevin Traverso
#* 

# Instalacion de librerias

# install.packages("cutoffR")
# install.packages("xts")
# install.packages("latticeExtra")
# install.packages("zoo")

# Cargando los paquetes

library(cutoffR)
library(xts)
library(latticeExtra)
library(zoo)

# Ingreso de datos

Data_pcp <- read.csv("./Data/2_Data_anual.csv",
                        header = T)

# Funcion de transformacion de datos xts

Data_xts <- function(Data_pcp){
  
  dates <- as.Date(
    x = Data_pcp[,1],
    format = "%Y-%m-%d"
    )
  
  Data_dxts <- xts::xts(
    x = Data_pcp[,-1], 
    order.by = dates
    )
  
  return(Data_dxts)
  
}

# Transformado datos en formato xts

Datos_xts <- Data_xts(Data_pcp)

# Graficando histogramas

xyplot(x = Datos_xts, 
       main = "Histograma de precipitaciones",
       xlab = "Fecha", 
       ylab = "Pcp[mm/dia]")

# Funcion de completacion cutoff

completa_datos <- function(Datos_xts){
  
  Datos_dtf <- data.frame(
    Datos_xts, 
    date = index(Datos_xts)
    )
  
  Completa_datos <- cutoff(
    data = Datos_dtf, 
    method = "correlation", 
    corr = "spearman",
    cutoff = 0.75
    )

  Datos_comp_xts <- xts::xts(
    x = Completa_datos, 
    order.by = index(Datos_xts))
  
  return(Datos_comp_xts)
  
}

Datos_comp <- completa_datos(Datos_xts)

# Graficando los datos completos

xyplot(x = Datos_comp, 
       col = "red", 
       main = "Histograma con informacion completa",
       xlab = "Fecha", 
       ylab = "Pcp[mm/dia]") +
  xyplot(x = Datos_xts)


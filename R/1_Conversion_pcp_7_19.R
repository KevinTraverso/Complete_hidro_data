#* **********************************************************************
#*
#* CONVERSION DE PRECIPITACIONES REGISTRADAS EN 7 Y 19
#* HORAS A PRECIPITACIONES DIARIAS
#* PLATAFORMA: ANDREA - ANA
#* 
#* conversion de datos pluviometricos
#* @autor: Kevin Traverso
#* 
#* **********************************************************************

# Instalacion de librerias

# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("hydroTSM")

# Cargando los paquetes *************************************************

library(readxl)
library(tidyverse)
library(lubridate)
library(plotly)
library(hydroTSM)

# Leer los datos de Pcp a 12 horas (FORMATO OBSERVATORIO ANA) **********

Datos_12h <- read_excel("./Data/Estacion_Taraco.xlsx",
                        skip = 18, 
                        col_names = c("Date", "Hr", "pmm"))

# Funcion de transformacion

Frm_12h <- function(Data){
  
  # Data <- Datos_12h
  Datos_12h <- as.data.frame(Data)
  Datos_12h[,1] <- dmy(Datos_12h[,1])
  Datos_a <- Datos_12h %>% spread(Hr, pmm)
  
  Pcp_d <- matrix(nrow = length(Datos_a$Date),
                  ncol = 1)
  
  for (i in 1:length(Datos_a$Date)) {
    Pcp_d[i,1] <- Datos_a[i+1,2] + Datos_a[i,3]
  }
  
  Datos_a <- cbind(Datos_a,Pcp_d)
  return(Datos_a)
  
}

Datos_a <- Frm_12h(Data = Datos_12h)

# Transformacion de datos de diarios a mensuales ***************************

mensual <- function(Data_pcp){
  
  Data_pcp <- zoo(Data_pcp$Pcp_d, 
                  order.by = Data_pcp[,1])
  Datos_m <- hydroTSM::daily2monthly(Data_pcp, 
                                     sum,
                                     FALSE)
  return(Datos_m)
  
}

datos_mensual <- mensual(Data_pcp = Datos_a)
plot(datos_mensual)

# Conversion de datos diarios a anuales pcp ********************************

anual <- function(Data_pcp){
  
  Data_pcp <- zoo(Data_pcp$Pcp_d,
                  order.by = Data_pcp[,1])
  Datos_a <- hydroTSM::daily2annual(Data_pcp,
                                    sum,
                                    FALSE)
  return(Datos_a)
  
}

datos_anual <- anual(Data_pcp = Datos_a)
plot(datos_anual)

# Grafico de precipitaciones diarias ***************************************

# grafico <- ggplot(data = Datos_a) + 
#   geom_line(mapping = aes(x=Date,
#                           y =Pcp_d), 
#             colour = "#40b8d0") + 
#   labs(x="Fecha",
#        y="Precipitacin diaria [mm]") +
#   ggtitle("Valores de precipitacion diarios") +
#   theme(axis.line = element_line(size=1, 
#                                  colour = "black"),
#         panel.grid.major = element_line(colour = "#d3d3d3"),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())
#   
# ggplotly(grafico)
  
# Guardado de datos ********************************************************

# 1. Datos diarios "convertidos"
write.csv(Datos_a,
          "./Salidas/Valores_diarios_Taraco.csv", 
          row.names = FALSE)

# 2. Datos mensuales
write.csv(datos_mensual,
          "./Salidas/Valores_mensuales_Taraco.csv", 
          row.names = T)

# 3. Datos anuales
write.csv(datos_anual,
          "./Salidas/Valores_anuales_Taraco.csv", 
          row.names = T)


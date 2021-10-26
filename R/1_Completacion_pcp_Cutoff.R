#* **********************************************************************
#*
#* COMPLETACION DE DATOS DE PRECIPITACIONES DIARIAS
#* PLATAFORMA: ANDREA - ANA
#* 
#* Completacion de datos pluviometricos, diarios, mensuales y
#* anuales
#* @autor: Kevin Traverso
#* 
#* **********************************************************************

# Instalacion de librerias

# install.packages("cutoffR")
# install.packages("xts")
# install.packages("latticeExtra")
# install.packages("zoo")
# install.packages("corrplot")
# install.packages("jsonlite")

# Cargando los paquetes *************************************************

library(cutoffR)
library(xts)
library(latticeExtra)
library(zoo)
library(corrplot)
library(jsonlite)

# Ingreso de datos ******************************************************

# Data_pcp <- read.csv("./Data/3_UH_Ramis.csv",
#                         header = T)

# Lectura de datos en formato JSON
Data_pcp <- fromJSON(txt = "./Data_json/SerieMensual.json")
cnames <- c("Est1", "Est2", "Est3")

# Funcion de transformacion de datos xts *********************************

Data_xts <- function(Data_pcp, temp){
  
  Data_dxts <- list()
  
  for (i in 1:length(Data_pcp)) {
    
    Data_pcp1 <- Data_pcp[[i]]
    a <- as.vector(
      t(as.matrix(Data_pcp1[,2:13]))
      )
    y <- seq.Date(
      as.Date(paste0(Data_pcp1[1,1], "-01", "-01")),
      as.Date(paste0(Data_pcp1[length(Data_pcp1[,1]),1],
                     "-12", "-31" )), by = temp
    )
    Data_dxts[[i]] <- xts::xts(
      x = a,
      order.by = y
    )
  }

 Data_dxts <- do.call(merge,lapply(Data_dxts,as.xts))
 colnames(Data_dxts) <- cnames
 return(Data_dxts)
 
}

# Data_xts <- function(Data_pcp){
#   
#   dates <- as.Date(
#     x = Data_pcp[,1],
#     format = "%Y-%m-%d"
#     )
#   
#   Data_dxts <- xts::xts(
#     x = Data_pcp[,-1], 
#     order.by = dates
#     )
#   
#   return(Data_dxts)
#   
# }

# Transformado datos en formato xts **************************************

Data_dxt1 <- Data_xts(Data_pcp = Data_pcp, temp = "month")
head(Data_dxt1)

# Datos_xts <- Data_xts(Data_pcp)

# Graficando histogramas

xyplot(x = Data_dxt1, 
       main = "Serie de tiempo de precipitaciones",
       xlab = "Fecha", 
       ylab = "Precipitacion[mm]")

# Asignacion de parametros y seleccion de periodo -------------------------

# Seleccion de metodo
# se recomienda correlation
# method_a <- data.frame("correlation", "number", "penalty")
# method_a1 <- method_a$X.correlation.

# Seleccion de corr "pearson", "spearman" y "kendall"
# se recomienda spearman
# corr_a <- data.frame("pearson", "spearman", "kendall")
# corr_a1 <- corr_a$X.spearman.

# Seleccion de valor cutoff
# se recomienda un valor de 0.75
# cutoff_a <- 0.75

# Seleccion de periodo de completacion de datos 
# Data_dxt2 <- window(Data_dxt1,
#                     start =  "1995-01-01",
#                     end =  "2017-12-31")

# -------------------------------------------------------------------------

# Funcion para el formato Json

CompletarDatos_cutoff <- function(
  
  Datos, 
  PeriodoInicio, PeriodoFin,
  Metodo, TipoC, cutoff_v
  
  ){
  
  Datos_xts <- window(
    Datos,
    start =  PeriodoInicio,
    end =  PeriodoFin
    )
  
  Datos_dtf <- data.frame(
    Datos_xts,
    date = index(Datos_xts)
  )
  
  Completa_datos <- cutoff(
    data = Datos_dtf,
    method = Metodo,
    corr = TipoC,
    cutoff = cutoff_v
  )
  
  Datos_comp_xts <- xts::xts(
    x = Completa_datos,
    order.by = index(Datos_xts)
  )
  
  Datos_comp <- list(
    index(Datos_comp_xts),
    Datos_xts,
    Datos_comp_xts
  )
  
  return(Datos_comp)

}

Datos_comp <- CompletarDatos_cutoff(
  Datos = Data_dxt1,
  PeriodoInicio = "1995-01-01",
  PeriodoFin = "2017-12-31",
  Metodo = "correlation",
  TipoC = "spearman",
  cutoff_v = 0.75
  )

# completa_datos <- function(Datos_xts){
#   
  # Datos_dtf <- data.frame(
  #   Datos_xts,
  #   date = index(Datos_xts)
  #   )
  # 
  # Completa_datos <- cutoff(
  #   data = Datos_dtf,
  #   method = method_a1,
  #   corr = corr_a1,
  #   cutoff = cutoff_a
  #   )
  # 
  # Datos_comp_xts <- xts::xts(
  #   x = Completa_datos,
  #   order.by = index(Datos_xts)
  #   )
  # 
  # return(Datos_comp_xts)
#   
# }

# Datos_comp <- completa_datos(Data_dxt2)
# 
# Salida_list <- list(index(Datos_comp),
#                     Data_dxt2,
#                     Datos_comp)

# Graficando los datos completos ****************************************

xyplot(x = Datos_comp, 
       col = "red", 
       main = "Serie de teimpo con informacion completa",
       xlab = "Fecha", 
       ylab = "Pcp[mm/dia]") +
  xyplot(x = Data_dxt2)

# Analisis de correlacion cruzada de datos completados ******************

corr1 <- function(Datos_comp){
  res <- cor(Datos_comp)
  
  a1 <- corrplot(res,  
                 order = 'AOE', 
                 type = 'upper',
                 method = 'number',
                 diag = T, 
                 tl.pos = 'd')
  
  a2 <- corrplot(res,
                 add = TRUE,
                 type = 'lower',
                 method = 'ellipse', 
                 order = 'AOE',
                 diag = FALSE,
                 tl.pos = 'n',
                 cl.pos = 'n')
  coor <- list(a1,a2)
  return(coor)
  
}

coor_pcp <- corr1(Datos_comp = Datos_comp)

# res <- cor(Datos_comp)
# 
# # Grafico 1
# a1 <- corrplot(res,  
#          order = 'AOE', 
#          type = 'upper',
#          method = 'number',
#          diag = T, 
#          tl.pos = 'd')
# a2 <- corrplot(res,
#          add = TRUE,
#          type = 'lower',
#          method = 'ellipse', 
#          order = 'AOE',
#          diag = FALSE,
#          tl.pos = 'n',
#          cl.pos = 'n')
# 
# correlatcion <- list(a1,a2)

# Grafico 2
corr2 <- function(Datos_comp){
  
  testRes = cor.mtest(Datos_comp, conf.level = 0.75)
  
  a1 <- corrplot(cor(Datos_comp),
                 p.mat = testRes$p,
                 method = 'circle',
                 type = 'lower',
                 insig='blank',
                 addCoef.col = 'black',
                 number.cex = 0.8,
                 order = 'AOE', 
                 diag = FALSE)
  
  a2 <- list(a1)
  
  return(a2)
}

corr2_pcp <- corr2(Datos_comp = Datos_comp)

# testRes = cor.mtest(Datos_comp, conf.level = 0.90)
# a1 <- corrplot(res,
#          p.mat = testRes$p,
#          method = 'circle',
#          type = 'lower',
#          insig='blank',
#          addCoef.col = 'black',
#          number.cex = 0.8,
#          order = 'AOE', 
#          diag = FALSE)


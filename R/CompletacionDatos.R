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
library(cutoffR)
library(xts)
# library(latticeExtra)
library(zoo)
library(corrplot)
library(jsonlite)
library(dplyr)
library(tidyr)
# library(tidyverse)

# Funcion de transformacion de datos xts *********************************
Data_xts <- function(Data_pcp, temp){
  
  cnames <- data.frame()
  
  for (i in 1:length(Data_pcp)) {
    
    cnames[1,i] <- paste0("Est_", i)
    
  }
  
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

# Funcion de Comletación de datos*********************************
CompletarDatos_cutoff <- function(TipoSerie, Datos, PeriodoInicio, PeriodoFin,Metodo, TipoC, cutoff_v)
{

  # Transformado datos en formato xts **************************************
  Data_dxt1 <- Data_xts(Data_pcp = Datos, temp = "month")
  head(Data_dxt1)
  # **************************************

  Datos_xts <- window(
    Data_dxt1,
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


  # Datos_comp <- list(
  #   index(Datos_comp_xts),
  #   Datos_xts,
  #   Datos_comp_xts
  # )

  #return(Datos_comp)

  Datos_comp <- list(
    format(as.Date(as.character(index(Datos_comp_xts)),
                   "%Y-%m-%d"), "%b%Y"),
    Datos_xts,
    Datos_comp_xts
  )
  Resultados_mtx <- tomatrix(Datos_comp = Datos_compM)

  coor_pcp <- corr1(Datos_comp = Datos_compM[[3]])

  Datos_comp <- list(Grafico=list(
    Periodo=as.character(index(Datos_comp_xts)),
    SerieImcompleta = as.matrix(Datos_xts),
    SerieCompleta = as.matrix(Datos_comp_xts)
    ),
    #Tabla=list(SerieImcompleta="",SerieCompleta="")
    Tabla=Resultados_mtx,
    Correlacion = coor_pcp
  )
  #as.matrix(Datos_xts)

}



tomatrix <- function(Datos_comp){

  m1 <- Datos_comp[[2]]
  m2 <- Datos_comp[[3]]

  m1_a <- list()
  m2_a <- list()

  for (i in 1:length(colnames(m1))) {
    
    m1_a[[i]] <- data.frame(
      Year=unique(format(index(m1[,i]),
                         format = "%Y")),
      t(matrix(round(m1[,i], digits=2),
               nrow=12)),
      row.names = unique(format(index(m1[,i]),
                                format = "%Y"))
    )
    
    m2_a[[i]] <- data.frame(
      Year=unique(format(index(m2[,i]),
                         format = "%Y")),
      t(matrix(round(m2[,i], digits=2),
               nrow=12)),
      row.names = unique(format(index(m2[,i]),
                                format = "%Y"))
    )
    
    colnames(m1_a[[i]]) <- c("year", month.abb)
    colnames(m2_a[[i]]) <- c("year", month.abb)
    
  }
  
  m3 <- list()
  
  for (i in 1:length(colnames(m1))) {
    m3[[i]] <- data.frame(
      Year=unique(format(index(m2[,i]),
                         format = "%Y")),
      na_if(round(m2_a[[i]][-1] - m1_a[[i]][-1] %>%
                    mutate_all(funs(replace_na(.,-0.001))),3), 
            0.000))
  }
  
  m4 <- list()
  
  for (i in 1:length(colnames(m1))) {
    
    Data_pcp1 <- m3[[i]]
    
    a <- as.vector(
      (as.matrix(Data_pcp1[,2:13]))
    )
    y <- seq.Date(
      as.Date(paste0(Data_pcp1[1,1], "-01", "-01")),
      as.Date(paste0(Data_pcp1[length(Data_pcp1[,1]),1],
                     "-12", "-31" )), by = temp
    )
    m4[[i]] <- xts::xts(
      x = a,
      order.by = y
    )
  }
  
  return(list(m1_a, m2_a, m3, m4))
  
}


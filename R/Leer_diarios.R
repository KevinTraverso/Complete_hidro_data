


# Cargando los paquetes *************************************************

library(cutoffR)
library(xts)
library(latticeExtra)
library(zoo)
library(corrplot)
library(jsonify)
library(dplyr)
library(tidyr)

serie_diaria <- jsonify::from_json("./Data_json/SerieDiaria4EstRamis_ultimo.json")
  
diarios_json <- function(Datos.diarios){
  
  cnames <- data.frame()
  
  for (i in 1:length(Datos.diarios)) {
    
    cnames[1,i] <- paste0("Est_", i)
    
  }
  
  a <- list()
  
  for (i in 1:length(Datos.diarios)) {
    
    a[[i]] <- xts::xts(x = Datos.diarios[[i]]$D,
                       order.by = as.Date(Datos.diarios[[i]]$F))
  }
  
  b <- do.call(merge,lapply(a,as.xts))
  
  colnames(b) <- cnames
  
  return(b)
  
}


diarios <- diarios_json(Datos.diarios = serie_diaria)






  
  
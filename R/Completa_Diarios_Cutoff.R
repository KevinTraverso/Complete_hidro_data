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
#* 
library(cutoffR)
library(xts)
# library(latticeExtra)
library(zoo)
library(corrplot)
#library(jsonlite)
library(dplyr)
library(tidyr)
library(RCurl)
# library(tidyverse)

# Funcion de transformacion de datos xts *********************************
Data_xts <- function(Data_pcp, NomEstaciones){
  
  Data_pcp <- jsonify::from_json(Data_pcp)

  cnames <- data.frame()
  
  for (i in 1:length(Data_pcp)) {
    cnames[1,i] <- paste0("Est_", i)
  }
  
  a <- list()
  
  for (i in 1:length(Data_pcp)) {
    a[[i]] <- xts::xts(x = Data_pcp[[i]]$D,
                       order.by = as.Date(Data_pcp[[i]]$F))
  }
  
  b <- do.call(merge,lapply(a,as.xts))
  colnames(b) <- cnames
  return(b)

}

datos <- Data_xts(Data_pcp = "./Data_json/SerieDiaria4EstRamis_ultimo.json")

# Funcion de Comletación de datos*********************************
CompletarDatos_cutoff <- function(Datos, PeriodoInicio, PeriodoFin,Metodo, TipoC, cutoff_v,NomEstaciones)
{

  # Transformado datos en formato xts **************************************
    Data_dxt1 <- Datos
  # head(Data_dxt1)
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
    method = "correlation",
    corr = "pearson",
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

  Datos_compM <- list(
    #format(as.Date(as.character(index(Datos_comp_xts)),"%Y-%m-%d"), "%b%Y"),
    Datos_comp_xts,
    Datos_xts,
    Datos_comp_xts
  )
  
  # Resultados_mtx <- tomatrix(Datos_comp = Datos_compM)

  png(tf1 <- tempfile(fileext = ".png"), type = "cairo");
  CDGraficoCorrelacion(Datos_comp = Datos_compM[[3]])
  dev.off()
  # Base64-encode file

  txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

  #data del gráfico
  # coor_pcp <- corr1(Datos_comp = Datos_compM[[3]])

  # Datos_comp <- list(Grafico=list(
  #   #Periodo=as.character(index(Datos_comp_xts)),
  #   Periodo=format(as.Date(as.character(index(Datos_comp_xts)),"%Y-%m-%d"), "%b%Y"),
  #   SerieImcompleta = as.matrix(Datos_xts),
  #   SerieCompleta = as.matrix(Datos_comp_xts),
  #   SerieSoloCompletado = Resultados_mtx[[4]]
  # ),
  # Tabla=list(Resultados_mtx[[1]],Resultados_mtx[[2]],Resultados_mtx[[3]]), #Resultados_mtx,
  # Correlacion = as.character(txt)#coor_pcp
  # )

  #as.matrix(Datos_xts)
  
  #############
  
  # Datos completos
  a <- list() 
  
  for (i in 1:length(Datos_comp_xts[1])) {
    # i = 1
    a[[i]] <- data.frame(F.= index(Datos_comp_xts[,i]),
                         D. = matrix(Datos_comp_xts[,i]))
  }
  
  # Datos incompletos recortados
  b <- list()
  
  for (j in 1:length(Datos_xts[1])) {
    # i = 1
    b[[j]] <- data.frame(F.= index(Datos_xts[,j]),
                         D. = matrix(Datos_xts[,j]))
  }
  
  # c <- list()
  # 
  # for (i in 1:length(Datos_comp_xts[1])) {
  #   
  #   m1 <- na_if(coredata(Datos_comp_xts[,i]-Datos_xts[,i]) %>% mutate_all(funs(replace_na(.,-0.001)))) 
  #   
  #   c[[i]] <- data.frame(
  #     F.= index(Datos_comp_xts[,i]),
  #     D.= m1
  #     )
  # }
 
  return(list(a,b))

}
  
comp_d <- CompletarDatos_cutoff(Datos = datos, 
                                PeriodoInicio = "1995-01-01",
                                PeriodoFin = "2008-12-31",
                                TipoC = "correlation", 
                                Metodo = "pearson", 
                                cutoff_v = 0.75 )


# tomatrix <- function(Datos_comp){

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
      t(as.matrix(Data_pcp1[,2:13]))
    )
    y <- seq.Date(
      as.Date(paste0(Data_pcp1[1,1], "-01", "-01")),
      as.Date(paste0(Data_pcp1[length(Data_pcp1[,1]),1],
                     "-12", "-31" )), by = "month"
    )
    m4[[i]] <- as.character(xts::xts(
      x = a,
      order.by = y
    ))
  }
  m4_auxi <- m4

  return(list(m1_a, m2_a, m3,m4_auxi))

}


#Grafico 1
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


CDGraficoCorrelacion <- function(Datos_comp){
  testRes = cor.mtest(Datos_comp, conf.level = 0.75)
  corrplot(cor(Datos_comp),
                 p.mat = testRes$p,
                 method = 'circle',
                 #title= 'Gráfico de correlación',
                 #mar=c(0,0,1,0),
                 type = 'lower',
                 insig='blank',
                 addCoef.col = 'black',
                 number.cex = 0.8,
                 order = 'AOE',
                 tl.col="black",
                 diag = FALSE)

}


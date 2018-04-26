library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
source("Codigo/funciones.R")
options(scipen = 9999999)

#################################Analisis por Hora Consistencia###############################################
load("dataGuardada/baseConsolidada.RData")
base = base %>% select(INT_PUNTO, OPERACION, FECHA, HORA, TIEMPO) %>% distinct(.keep_all = T)

nuevaBase = data.frame()
listaPuntos = sort(unique(base$INT_PUNTO))
punto = listaPuntos[1]
for (punto in listaPuntos){
  temp = base %>% filter(INT_PUNTO == punto) %>% arrange(TIEMPO)
  minFecha = min(temp$TIEMPO)
  maxFecha = max(temp$TIEMPO)
  minFecha = strftime(as.POSIXlt(minFecha, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:00:00")
  maxFecha = strftime(as.POSIXlt(maxFecha, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:00:00")
  secuencia = seq(from = as.POSIXlt(minFecha), to = as.POSIXlt(maxFecha), by = "hour") %>% as.data.frame()
  names(secuencia) = c("TIEMPO")
  secuencia$TIEMPO = as.character(secuencia$TIEMPO)
}
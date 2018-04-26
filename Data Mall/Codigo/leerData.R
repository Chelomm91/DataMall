library(dplyr)
library(tidyr)
source("Codigo/funciones.R")

totalClientes = read.csv("data/TotClientes.csv")
totalClientes = totalClientes %>% select(INTERNO, NOMBRE, INFORMACION,IDENTIFICADOR, HABILITADO, HOMOLOGADO, DIA_OPERACION)
totalClientes = totalClientes %>% mutate(HABILITADO = if_else(grepl("S", HABILITADO), 1, 0))
totalClientes = totalClientes %>% mutate(HOMOLOGADO = if_else(grepl("S", HOMOLOGADO), 1, 0))
totalClientes = totalClientes %>% mutate(DIA_OPERACION = if_else(grepl("CAMBIA", DIA_OPERACION), 1, 0))

totalPuntos = read.csv("data/TotClientesPuntos.csv")
totalPuntos = totalPuntos %>% select(INTERNO, INT_CLIENTE, PREFIJO, INFORMA_CIERRE, CONTROLA_CIERRE, PUERTO, INFORMACION, INSTALACION, SINTOMA, NUMERACION, HOMOLOGADO)
totalPuntos = totalPuntos %>% mutate(CONTROLA_CIERRE = if_else(grepl("S", CONTROLA_CIERRE), 1, 0))

totalClientes = totalClientes %>% select(-c(INFORMACION, HOMOLOGADO))
totalPuntos = totalPuntos %>% select(-c(INFORMA_CIERRE, CONTROLA_CIERRE, HOMOLOGADO))

'totalEstados = read.csv("data/tot_clientes_puntos_20180425.csv")
totalEstados = totalEstados %>% mutate(FECHA = strftime(as.POSIXct(FECHA), format = "%Y-%m-%d"))
totalEstados = totalEstados %>% separate(col = HORA,into = c("FECHA2","HORA"), sep = " ")
totalEstados = totalEstados %>% select(-c(FECHA2))
totalEstados = totalEstados %>% distinct(INTPUNTO, OPERACION, FECHA, HORA, DRIVER, TRANSFER, FEC_REGISTRACION, HOR_REGISTRACION, .keep_all = T)
save(totalEstados, file = "dataGuardada/totalEstados.RData")'

'totalEstados = read.csv("data/tot_clientes_puntos_20180425.csv")
totalEstados = totalEstados %>% select(-c(INTERNO)) %>% distinct(.keep_all = T)
totalEstados = totalEstados %>% mutate(FECHA = strftime(as.POSIXct(FECHA), format = "%Y-%m-%d"))
totalEstados = totalEstados %>% mutate(HORA = strftime(as.POSIXct(HORA, format = "%Y-%m-%dT%H:%M:%S.000Z"), format = "%H:%M:%S"))
totalEstados = totalEstados %>% group_by(INT_PUNTO) %>% arrange(FECHA,HORA) %>% mutate(HORA2 = lag(HORA)) %>% ungroup() %>% as.data.frame()
totalEstados = totalEstados %>% mutate(DIFERENCIAHORA = difftime(as.POSIXlt(HORA, format = "%H:%M:%S"),as.POSIXlt(HORA2, format = "%H:%M:%S"),units = "mins"))'

load("dataGuardada/totalEstados.RData")

totalPuntos = totalPuntos %>% rename(INT_PUNTO = INTERNO)
totalPuntos = totalPuntos %>% arreglarNumeors(c(1,2))

totalClientes = totalClientes %>% rename(INT_CLIENTE = INTERNO)
totalClientes = totalClientes %>% arreglarNumeors(c(1))

totalEstados = totalEstados %>% rename(INT_ESTADO = INTERNO)
totalEstados = totalEstados %>% arreglarNumeors(c(1,2))

base = totalPuntos %>% full_join(totalClientes, by = c("INT_CLIENTE"))
base = totalEstados %>% full_join(base, by =c("INT_PUNTO"))

sinData = base %>% filter(is.na(INT_ESTADO))
base = base %>% filter(!is.na(INT_ESTADO))
base = base %>% mutate(TIEMPO = strftime(as.POSIXlt(paste0(FECHA," ",HORA)), format = "%Y-%m-%d %H:%M:%S"))

save(sinData, base, file = "dataGuardada/baseConsolidada.RData")

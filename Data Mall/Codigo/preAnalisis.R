library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
source("Codigo/funciones.R")
options(scipen = 9999999)


##SINTOMA##########
load("dataGuardada/baseConsolidada.RData")
activo = base %>% select(INT_PUNTO, SINTOMA) %>% distinct(.keep_all = T)
activo$SINTOMA = as.character(activo$SINTOMA)
activo = activo %>% mutate(SINTOMA=if_else(grepl('EN REVISI',SINTOMA), "REVISION", SINTOMA))
activo = activo %>% mutate(SINTOMA = if_else(SINTOMA == "BAJA", 0,1))
base = base %>% select(-c(SINTOMA))


load("dataGuardada/baseConsolidada.RData")
dataGeneral = base
##Analisis por Hora Consistencia
base = dataGeneral %>% select(INT_ESTADO, INT_PUNTO, OPERACION, FECHA, HORA, TIEMPO)

base = base %>% group_by(INT_PUNTO) %>% arrange(TIEMPO) %>% ungroup() %>% as.data.frame()
base = base %>% select(-c(INT_ESTADO)) %>% distinct(.keep_all = T)

################################NUMERO ACTUALIZACIONES DIARIAS############################
temp = base %>% select(OPERACION) %>% unique() %>% mutate(Numero = 1:n())
base = base %>% left_join(temp, by = c("OPERACION")) %>% rename(NUMERO_OPERACION = Numero)
base = base %>% filter(OPERACION == "ACTUALIZACION")
base = base %>% group_by(INT_PUNTO, FECHA) %>% summarise(TOTAL = n()) %>% ungroup() %>% as.data.frame()

names(base) = c("int_punto","fecha","total")

base = base %>% group_by(int_punto) %>% mutate(fecha2 = lag(fecha)) %>% ungroup()
base = base %>% mutate(Resta = as.Date(fecha)-as.Date(fecha2))
base = base %>% mutate(Resta = as.numeric(Resta))
base = base %>% as.data.frame()

masDias = base %>% filter(Resta > 1)
actDiarias = base

#############################INDICADOR ACTUALIZACION############################################

##Analisis por Hora Consistencia
base = dataGeneral %>% select(INT_ESTADO, INT_PUNTO, OPERACION, FECHA, HORA, TIEMPO)

base = base %>% group_by(INT_PUNTO) %>% arrange(TIEMPO) %>% ungroup() %>% as.data.frame()
base = base %>% select(-c(INT_ESTADO)) %>% distinct(.keep_all = T)

base = base %>% select(-c(OPERACION))
base = base %>% rename(TOTAL = HORA)
base = base %>% mutate(HORA = strftime(as.POSIXlt(TOTAL, format = "%H:%M:%S"), format = "%H"))
base = base %>% select(INT_PUNTO, TIEMPO, HORA)
base = base %>% mutate(TIEMPO = strftime(as.POSIXlt(TIEMPO, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:00:00"))
base = base %>% distinct(.keep_all = T)
base = base %>% semi_join(activo %>% filter(SINTOMA == 1), by = c("INT_PUNTO"))

listaPuntos = unique(base$INT_PUNTO)

punto = 276
popularLineal = data.frame()
popularArima = data.frame()
cont = 1

base = base %>% filter(TIEMPO >= as.POSIXlt("2018-04-19 00:00:00", format = "%Y-%m-%d %H:%M:%S"))
for (punto in listaPuntos){
  print(paste0(punto,"    ", cont," de ", length(listaPuntos)))
  cont = cont+1
  temp = base %>% filter(INT_PUNTO == punto)
  minFecha = min(temp$TIEMPO)
  maxFecha = max(temp$TIEMPO)
  
  minFecha = strftime(as.POSIXlt(minFecha, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:00:00")
  maxFecha = strftime(as.POSIXlt(maxFecha, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:00:00")
  if (nrow(temp)>0){
    secuencia = seq(from = as.POSIXlt(minFecha), to = as.POSIXlt(maxFecha), by = "hour") %>% as.data.frame()
    names(secuencia) = c("TIEMPO")
    secuencia$TIEMPO = as.character(secuencia$TIEMPO)
    temp = secuencia %>% left_join(temp, by = c("TIEMPO"))
    temp = temp %>% mutate(HORA = strftime(as.POSIXlt(TIEMPO, format = "%Y-%m-%d %H:%M:%S"), format = "%H"))
    temp = temp %>% mutate(CON = if_else(is.na(INT_PUNTO), 0, 1))
    temp = temp %>% mutate(INT_PUNTO = punto) %>% mutate(Numero = 1:n())
    graf = ggplot(data = temp, mapping = aes(x = Numero, y = CON))+geom_line(color = "blue", mapping = aes(group = "identity"))+theme_classic()+
      xlab('Fecha')+ylab('Conexion')+theme(axis.text.x = element_text(angle = 90))
    ggsave(filename = paste0("Grafica/",punto,".png"), plot = graf, width = 10, height = 7)
  }
 
  
  'testSeaso = testSeasonal(temp)
  if (testSeaso < 0.05){
    pred = prediccionLineal(temp, punto, 12) %>% as.data.frame()
    names(pred) = c("valor")
    pred = pred %>% mutate(Punto = punto, filas = nrow(temp), test = 1)
    popularLineal = popularLineal %>% plyr::rbind.fill(pred)
    
    pred = prediccionArima(temp, punto) %>% as.data.frame()
    names(pred) = c("valor")
    pred = pred %>% mutate(Punto = punto, filas = nrow(temp), test = 1)
    popularArima = popularArima %>% plyr::rbind.fill(pred)
  }else{
    pred = 0 %>% as.data.frame()
    names(pred) = c("valor")
    pred = pred %>% mutate(Punto = punto, filas = nrow(temp), test = 0)
    popularLineal = popularLineal %>% plyr::rbind.fill(pred)
    
    pred = 0 %>% as.data.frame()
    names(pred) = c("valor")
    pred = pred %>% mutate(Punto = punto, filas = nrow(temp), test = 0)
    popularArima = popularArima %>% plyr::rbind.fill(pred)
  }'
  
  
  
}

popularLineal = popularLineal %>% mutate(Porce = (valor/(filas*0.06) %>% as.integer())*100)
popularArima = popularArima %>% mutate(Porce = (valor/(filas*0.06) %>% as.integer())*100)

print(popularLineal)
print(popularArima)

write.csv(popularArima, file = "resultados_preliminares.csv", row.names = F)

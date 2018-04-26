library(dplyr)
library(tidyr)
library(ggplot2)
source("Codigo/funciones.R")
load("dataGuardada/baseConsolidada.RData")
options(scipen = 9999999)

load("dataGuardada/baseConsolidada.RData")
activo = base %>% select(INT_PUNTO, SINTOMA) %>% distinct(.keep_all = T)
activo$SINTOMA = as.character(activo$SINTOMA)
activo = activo %>% mutate(SINTOMA=if_else(grepl('EN REVISI',SINTOMA), "REVISION", SINTOMA))
activo = activo %>% mutate(SINTOMA = if_else(SINTOMA == "BAJA", 0,1))
activo = activo %>% filter(SINTOMA == 1)
base = base %>% select(-c(SINTOMA))



#########################HISOGRAMA######################################
base = base %>% group_by(INT_PUNTO) %>% arrange(TIEMPO) %>% ungroup() %>% as.data.frame()
base = base %>% filter(OPERACION == "ACTUALIZACION")
base = base %>% select(-c(INT_ESTADO))
base = base %>% select(c(INT_PUNTO, TIEMPO)) %>% distinct(.keep_all = T)

base = base %>% semi_join(activo, by = c("INT_PUNTO"))

puntos = unique(base$INT_PUNTO)
punto = 267

popular = data.frame()
cont = 1



for (punto in puntos){
  print(paste0(punto," ",cont," de ", "122:  ",cont/122))
  cont = cont + 1
  temp = base %>% filter(INT_PUNTO == punto) %>% arrange(TIEMPO)
  temp = temp %>% select(TIEMPO) %>% mutate(TIEMPO2 = lag(TIEMPO))
  temp = temp %>% mutate(DIF = difftime(as.POSIXlt(TIEMPO),as.POSIXlt(TIEMPO2),units = 'mins'))
  temp = temp %>% mutate(DIF = as.numeric(DIF))
  temp = temp %>% arrange(desc(DIF))
  if (nrow(temp)>0){
    print("ENTRO")
    diferencias = temp %>% mutate(DIF = as.numeric(DIF)) %>% filter(!is.na(DIF))
    diferencias = diferencias %>% mutate(fila = 1:n())
    diferencias$punto = punto
    temp$DIF = as.numeric(temp$DIF)
    x = quantile(temp$DIF, na.rm = T)
    nombres = names(x)
    x = rbind(data.frame(matrix(x, nrow = 1)) %>% as.data.frame())
    names(x) = nombres
    
    y = quantile(temp$DIF, na.rm = T, probs = seq(0,1,length = 11))
    nombres = names(y)
    y = rbind(data.frame(matrix(y, nrow = 1)) %>% as.data.frame())
    names(y) = nombres
    y = y %>% select(-c(`0%`, `50%`,`100%`))
    x = x %>% cbind(y)
    
    y = quantile(temp$DIF, na.rm = T, probs = seq(0.91,0.99,length = 9))
    nombres = names(y)
    y = rbind(data.frame(matrix(y, nrow = 1)) %>% as.data.frame())
    names(y) = nombres
    x = x %>% cbind(y)
    
    x$PUNTO = punto
    popular = popular %>% rbind(x)
  }
}

write.csv(popular, file = "cuartiles2.csv", row.names = F)

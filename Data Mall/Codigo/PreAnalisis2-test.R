library(dplyr)
library(tidyr)
source("Codigo/funciones.R")
load("dataGuardada/baseConsolidada.RData")
options(scipen = 9999999)

##SINTOMA##########
activo = base %>% select(INT_PUNTO, SINTOMA) %>% distinct(.keep_all = T)
activo$SINTOMA = as.character(activo$SINTOMA)
activo = activo %>% mutate(SINTOMA=if_else(grepl('EN REVISI',SINTOMA), "REVISION", SINTOMA))
activo = activo %>% mutate(SINTOMA = if_else(SINTOMA == "BAJA", 0,1))
base = base %>% select(-c(SINTOMA))


#########################HISOGRAMA######################################
base = base %>% group_by(INT_PUNTO) %>% arrange(TIEMPO) %>% ungroup() %>% as.data.frame()
base = base %>% filter(OPERACION == "ACTUALIZACION")
base = base %>% select(-c(INT_ESTADO))
base = base %>% select(c(INT_PUNTO, TIEMPO)) %>% distinct(.keep_all = T)

puntos = unique(base$INT_PUNTO)
punto = puntos[1]

popular = data.frame()
cont = 1
for (punto in puntos){
  print(paste0(punto," ",cont," de ", "150:  ",cont/150))
  cont = cont + 1
  temp = base %>% filter(INT_PUNTO == punto) %>% arrange(TIEMPO)
  temp = temp %>% select(TIEMPO) %>% mutate(TIEMPO2 = lag(TIEMPO))
  temp = temp %>% mutate(DIF = difftime(as.POSIXlt(TIEMPO),as.POSIXlt(TIEMPO2),units = 'mins'))
  temp$DIF = as.numeric(temp$DIF)
  cuartiles = quantile(temp$DIF, na.rm = T)
  q0 = cuartiles[1]
  q1 = cuartiles[2]
  q2 = cuartiles[3]
  q3 = cuartiles[4]
  q4 = cuartiles[5]
  minimo = min(temp$TIEMPO)
  maximo = max(temp$TIEMPO)
  cuartiles = data.frame(punto, q0,q1,q2,q3,q4, mean(temp$DIF, na.rm = T), sd(temp$DIF, na.rm = T)/(mean(temp$DIF, na.rm = T)))
  row.names(cuartiles) = c(0)
  names(cuartiles) = c("PUNTO","Q0","Q1","Q2","Q3","Q4","Media","CoeficienteVar")
  popular = popular %>% plyr::rbind.fill(cuartiles)
}

popular = popular %>% select(-c(Q0))
popular = popular %>% left_join(activo, by =c("PUNTO" = "INT_PUNTO"))
dataCoeficientes = popular


popular$Q1 = scale(popular$Q1)[1:nrow(popular)]
popular$Q2 = scale(popular$Q2)[1:nrow(popular)]
popular$Q3 = scale(popular$Q3)[1:nrow(popular)]
popular$Q4 = scale(popular$Q4)[1:nrow(popular)]
popular$Media = scale(popular$Media)[1:nrow(popular)]
popular$CoeficienteVar = scale(popular$CoeficienteVar)[1:nrow(popular)]
data = popular[2:ncol(popular)]

set.seed(431465) 

k.max = 15
wss = sapply(1:k.max, 
             function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

res = kmeans(x = data, centers = 4, iter.max = 50, nstart = 200)
dataCoeficientes$Cluster = res$cluster
write.csv(dataCoeficientes, "base.csv", row.names = F)


resumen = data.frame()
maximos = dataCoeficientes %>% group_by(Cluster) %>% summarise(Q1 = max(Q1), Q2 = max(Q2), Q3 = max(Q3), Q4 = max(Q4), CoeficienteVar = max(CoeficienteVar), SINTOMA = max(SINTOMA), Numero = n())
minimos = dataCoeficientes %>% group_by(Cluster) %>% summarise(Q1 = min(Q1), Q2 = min(Q2), Q3 = min(Q3), Q4 = min(Q4),CoeficienteVar = min(CoeficienteVar), SINTOMA = min(SINTOMA), Numero = n())
media = dataCoeficientes %>% group_by(Cluster) %>% summarise(Q1 = mean(Q1), Q2 = mean(Q2), Q3 = mean(Q3), Q4 = mean(Q4),CoeficienteVar = mean(CoeficienteVar), SINTOMA = mean(SINTOMA), Numero = n())

maximos$Tipo = "maximo"
minimos$Tipo = "minimo"
media$Tipo = "media"
resumen = resumen %>% plyr::rbind.fill(maximos) %>% plyr::rbind.fill(minimos) %>% plyr::rbind.fill(media)
resumen = resumen %>% arrange(Cluster, Tipo)

write.csv(resumen, "temp.csv", row.names = F)

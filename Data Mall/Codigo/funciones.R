arreglarNumeors = function(base, columnas){
  for (i in columnas){
    base[,i] = gsub(pattern = ",", replacement = "", x = base[,i])
    base[,i] = as.numeric(base[,i])
  }
  return(base)
}

prediccionLineal = function(temp, punto, coefFour){
  tomar = nrow(temp)-(nrow(temp)*0.06) %>% as.integer()
  serie<-ts(temp$CON[1:tomar],frequency = 24)
  desc<-fourier(serie,coefFour)
  for (k in 1:ncol(desc)){
    colnames(desc)[k] = paste0("x",k)
  }
  desc<-cbind(y=temp$CON[1:tomar],desc)%>%as.data.frame()
  nombres = c("y~x1",names(desc)[2:length(names(desc))])
  nombres = paste0(nombres, collapse = "+")
  eq<-as.formula(nombres)
  reg<-lm(eq,desc)
  
  serie2<-ts(temp$CON[(tomar+1):nrow(temp)],frequency = 24)
  desc2<-fourier(serie,coefFour,h=nrow(temp)-tomar)
  for (k in 1:ncol(desc2)){
    colnames(desc2)[k] = paste0("x",k)
  }
  desc2<-desc2%>%as.data.frame()
  prediccion<-predict(reg,desc2)
  
  prediccion = cbind(prediccion,con=temp$CON[(tomar+1):nrow(temp)]) %>% as.data.frame()
  prediccion = prediccion %>% mutate(prediccion = if_else(prediccion < 0.5, 0, 1))
  prediccion = prediccion %>% mutate(error = abs(prediccion-con))
  prediccion$Tiempo = temp$TIEMPO[(tomar+1):nrow(temp)]
  return(sum(prediccion$error))
}

prediccionArima = function(temp, punto, coefFour){
  tomar = nrow(temp)-(nrow(temp)*0.06) %>% as.integer()
  
  ultimas = temp[(tomar+1):nrow(temp),]
  primeras = temp[0:tomar,]
  serie = ts(primeras$CON, frequency = 24)
  arima = auto.arima(serie, xreg = fourier(serie, 4), stationary = T, stepwise = F, allowdrift = F, allowmean = F, parallel = T, seasonal = F)
  predict(object = arima, newxreg = fourier(serie, 4, h = nrow(temp)-tomar))->prediccion
  
  resultados= prediccion$pred %>% as.data.frame()
  names(resultados) = c("Predecido")
  resultados = resultados %>% mutate(Predecido = if_else(Predecido>0.5, 1, 0))
  resultados= cbind(resultados, con=ultimas$CON)
  resultados =cbind(resultados, fecha = ultimas$TIEMPO)
  resultados = resultados %>% mutate(error = abs(Predecido - con))
  return(sum(resultados$error))
}

testSeasonal = function(temp){
  try({serie<-ts(temp$CON,frequency = 24)
  fit1 <- ets(serie)
  fit2 <- ets(serie,model="ANN")
  deviance <- 2*c(logLik(fit1) - logLik(fit2))
  df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
  #P value
  print(1-pchisq(deviance,df))
  return(1-pchisq(deviance,df))}, silent = T)
  return(1)
}
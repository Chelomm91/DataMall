
inicio = Sys.time()
ultimas200 = temp[(nrow(temp)-199):nrow(temp),]
primeras = temp[0:(nrow(temp)-200),]
serie = ts(primeras$CON, frequency = 24)
arima = auto.arima(serie, xreg = fourier(serie, 4), stationary = T, stepwise = F, allowdrift = F, allowmean = F, parallel = T, seasonal = F)
predict(object = arima, newxreg = fourier(serie, 4, h = 200))->prediccion
print(Sys.time()-inicio)

resultados6= prediccion$pred %>% as.data.frame()
names(resultados6) = c("Predecido")
resultados6 = resultados6 %>% mutate(Predecido = if_else(Predecido>0.5, 1, 0))
resultados6 = cbind(resultados6, con=ultimas200$CON)
resultados6 =cbind(resultados6, fecha = ultimas200$TIEMPO)
resultados6 = resultados6 %>% mutate(error = abs(Predecido - con))
print(sum(resultados6$error))
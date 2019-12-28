


gg_miss_upset(train)

#Variables Temp9am & Temp3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Temp9am, Temp3pm) %>% marginplot()



#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
#Parece que tiene sentido:
train %>% select(Temp9am, Temp3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train, variable=c("Temp9am","Temp3pm"))
#test_imputed <- kNN(test, variable=c("Temp9am","Temp3pm"))


#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Temp9am, na.rm = T), col=2, main="Temp9am")
lines(density(train_imputed$Temp9am), col=3)
plot(density(train$Temp3pm, na.rm = T), col=2, main="Temp3pm")
lines(density(train_imputed$Temp3pm), col=3) 

#Variables MinTemp & MaxTemp: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(MinTemp, MaxTemp) %>% marginplot()
#Parece que tiene sentido:
train %>% select(MinTemp, MaxTemp) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train, variable=c("MaxTemp","MinTemp"))
#test_imputed <- kNN(test, variable=c("MaxTemp","MinTemp"))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$MaxTemp,na.rm = T),col=2,main="MaxTemp")
lines(density(train_imputed$MaxTemp),col=3)
plot(density(train$MinTemp,na.rm = T),col=2,main="MinTemp")
lines(density(train_imputed$MinTemp),col=3)

#Variables Pressure3pm & Pressure9am: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Pressure3pm, Pressure9am) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Pressure3pm, Pressure9am) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Pressure3pm","Pressure9am"))
#test_imputed <- kNN(test_imputed, variable=c("Pressure3pm","Pressure9am"))


#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Pressure3pm,na.rm = T),col=2,main="Pressure3pm")
lines(density(train_imputed$Pressure3pm),col=3)
plot(density(train$Pressure9am,na.rm = T),col=2,main="Pressure9am")
lines(density(train_imputed$Pressure9am),col=3)

#Variables Humidity9am & Humidity3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Humidity3pm, Humidity9am) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Humidity3pm, Humidity9am) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Humidity3pm","Humidity9am"))
#test_imputed <- kNN(test_imputed, variable=c("Humidity3pm","Humidity9am"))


#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Humidity3pm,na.rm = T),col=2,main="Humidity3pm")
lines(density(train_imputed$Humidity3pm),col=3)
plot(density(train$Humidity9am,na.rm = T),col=2,main="Humidity9am")
lines(density(train_imputed$Humidity9am),col=3)

trainSelect2 <- train %>% select(RainToday, Rainfall)
gg_miss_upset(trainSelect2)

#Variables Humidity9am & Rainfall: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Humidity9am, Rainfall) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Humidity9am, Rainfall) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Humidity9am","Rainfall"))
# test_imputed <- kNN(test_imputed, variable=c("Humidity9am","Rainfall"))





train_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp", "Humidity3pm_imp", "Pressure3pm_imp","Pressure9am_imp")] <- list(NULL)

imputacion_knn_test<-function(train_imputed, test){
  train_imputed["dataset"] <- 'train'
  test["dataset"] <- 'test'
  dataset_knn = rbind(train_imputed, test) 
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Temp9am","Temp3pm"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("MaxTemp","MinTemp"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Humidity3pm","Humidity9am"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Pressure3pm","Pressure9am"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Humidity9am","Rainfall"))
  train_imputed = subset(dataset_knn_imputed, dataset == 'train')
  test_imputed = subset(dataset_knn_imputed, dataset == 'test')
  test_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp", "Humidity3pm_imp", "Pressure3pm_imp","Pressure9am_imp","dataset")] <- list(NULL)
  return (test_imputed) }

test_imputed = imputacion_knn_test(train_imputed, test)





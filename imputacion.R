#Detección, tratamiento e imputación de datos faltantes
#Valores Faltantes
# Visualización de valores faltantes
#Variables con más valores faltantes: Sunshine, Evaporation, Cloud3pm, Cloud9am, Pressure3pm, Pressure9am
var_Temp = train %>% select(MinTemp, MaxTemp, Location, Rainfall, Temp9am, Temp3pm)
var_cloud = train %>% select(Cloud9am, Cloud3pm)

aggr_plot <- aggr(var_Temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_Temp), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot

aggr_plot <- aggr(var_cloud, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_cloud), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))




#Variables Temp9am & Temp3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Temp9am, Temp3pm) %>% marginplot()
#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
#Parece que tiene sentido:
train %>% select(Temp9am, Temp3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train, variable=c("Temp9am","Temp3pm"))
aggr(train_imputed%>% select(Temp9am_imp, Temp3pm_imp, Temp9am, Temp3pm), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

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
train_imputed %>% select(MinTemp, MaxTemp) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("MaxTemp","MinTemp"))
aggr(train_imputed%>% select(MinTemp, MaxTemp, MaxTemp_imp, MinTemp_imp), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$MaxTemp,na.rm = T),col=2,main="MaxTemp")
lines(density(train_imputed$MaxTemp),col=3)
plot(density(train$MinTemp,na.rm = T),col=2,main="MinTemp")
lines(density(train_imputed$MinTemp),col=3)

#Variables Humidity9am & Rainfall: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Humidity9am, Rainfall) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Humidity9am, Rainfall) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Humidity9am","Rainfall"))
aggr(train_imputed%>% select(Humidity9am, Rainfall, Humidity9am_imp, Rainfall_imp), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Humidity9am,na.rm = T),col=2,main="Humidity9am")
lines(density(train_imputed$Humidity9am),col=3)
plot(density(train$Rainfall,na.rm = T),col=2,main="Rainfall")
lines(density(train_imputed$Rainfall),col=3)

# Checkea numero de NA
(cols_withNa <- apply(train_imputed, 2, function(x) sum(is.na(x))))

#Aplicamos el mismo método para los nules de test. Para ello hay que imputarlos junto con el dataframe de train.
train_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp")] <- list(NULL)

imputacion_knn_test<-function(train_imputed, df_test){
  train_imputed["dataset"] <- 'train'
  test["dataset"] <- 'test'
  dataset_knn = rbind(train_imputed, test) 
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Temp9am","Temp3pm"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("MaxTemp","MinTemp"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Humidity9am","Rainfall"))
  train_imputed = subset(dataset_knn_imputed, dataset == 'train')
  test_imputed = subset(dataset_knn_imputed, dataset == 'test')
  test_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp")] <- list(NULL)
  return (test_imputed) }

test_imputed = imputacion_knn_test(train_imputed, df_test)

#Transformaciones de variables cuantitativas
#La única que no parece seguir una distribución normal es la variable RainFall
#No se le puede aplicar logaritmos porque hay nulos.

p1 <- train_imputed %>% select(Rainfall, Season) %>%
  na.omit() %>%
  ggplot(aes(x=Rainfall, colour=Season)) +
  geom_density()

p2 <- train_imputed %>% mutate(inv_Rainfall = 1/ (train_imputed$Rainfall+ 1)) %>%
  select(Season, inv_Rainfall) %>%
  na.omit() %>%
  ggplot(aes(x=inv_Rainfall, colour=Season)) +
  geom_density()

p3 <- train_imputed %>% mutate(log10_Rainfall = log10(Rainfall + 0.01)) %>%  #+ 1.01
  select(Season, log10_Rainfall) %>%
  na.omit() %>%
  ggplot(aes(x=log10_Rainfall, colour=Season)) +
  geom_density()

hist(train_imputed$Rainfall)
hist(log10(train_imputed$Rainfall))
hist(log10(train_imputed$Rainfall) + 1)
#Sigue sin quedar una distribución normal...
grid.arrange(p1, p2, p3, nrow = 1)

train_imputed$LogRainfall<- (log(train_imputed$Rainfall + 0.011)) #log(train_imputed$Rainfall+1.01)

train_imputed %>% select(Season, Rainfall) %>%
  na.omit() %>%
  symbox(~ Rainfall, data = .)

qqnorm(train_imputed$LogRainfall, ylab="LogRainfall")
qqline(train_imputed$LogRainfall, col="red")

#Resto de variables: siguen distribución normal
qqnorm(train_imputed$MinTemp, ylab="MinTemp")
qqline(train_imputed$MinTemp, col="red")

qqnorm(train_imputed$MaxTemp, ylab="MaxTemp")
qqline(train_imputed$MaxTemp, col="red")

qqnorm(train_imputed$Temp3pm, ylab="Temp3pm")
qqline(train_imputed$Temp3pm, col="red")

qqnorm(train_imputed$Temp9am, ylab="Temp9am")
qqline(train_imputed$Temp9am, col="red")




# Evaporation
train$Evaporation[which(is.na(train$Evaporation))] <- mean(train$Evaporation,na.rm = TRUE)

#Imputacion WindGustSpeed
a <- data.frame(WindSpeed9am= train$WindSpeed9am, WindSpeed3pm= train$WindSpeed3pm)
means <- rowMeans(a, na.rm=TRUE)
train$WindGustSpeed[which(is.na(train$WindGustSpeed))] <- means

# Imputacion RainToday y RainTomorrow
rain_today <- train$RainToday
rain_today[is.na(rain_today)] <- "No"

rain_today <- ifelse(rain_today == "Yes", 1, 0)
train$RainToday = as.numeric(rain_today)

rain_tomorrow <- train$RainTomorrow
rain_tomorrow <- ifelse(rain_tomorrow == "Yes", 1, 0)
train$RainTomorrow = as.numeric(rain_tomorrow)


# check NA
train %>%
  summarise_each(list(~ sum(is.na(.)) / length(.) * 100)) %>%
  t()

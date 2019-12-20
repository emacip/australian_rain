  #https://www.kaggle.com/jsphyg/weather-dataset-rattle-package
#Lectura de los datos
library(ggplot2)
library(dplyr)
library(tidyverse)
library(VIM)
library(wesanderson)
library(GGally)
library(corrplot)
library(mice)
library(egg)
library(kableExtra)
library(car)
library(fastDummies)
library(caret)
weatherAUS <- weatherAUS_2
  #read.csv("~/Escritorio/MASTER/FUNDAMENTOS DE ANÁLISIS DE DATOS/practica_FAD/australian_rain/weatherAUS_2.csv")
dim(weatherAUS)

#Dividir Train / Test
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(weatherAUS)), size = 0.80 * nrow(weatherAUS))
train <- weatherAUS[train_ind, ]
temp <- weatherAUS[-train_ind, ]
test_val_ind<- sample(seq_len(nrow(temp)), size = 0.50 * nrow(temp))
test <- temp[test_val_ind, ]
validation <- temp[-test_val_ind, ]
dim(train)
dim(test)
dim(validation)

#Estudio variables
#Location
var_location = train$Location
length(levels(var_location))
ggplot(data=train, aes(x=Location, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))
#comparación con el target
ggplot(train, aes(Location)) + geom_bar(aes(stat="identity", fill = RainTomorrow), position = position_dodge(0.9)) + theme(axis.text.x=element_text(angle=90,hjust=1)) + scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4"))

#tabla de contingencia
with(train, table(Location, RainTomorrow))
with(train, table(Location, RainToday))

#MinTemp
var_MinTemp = train$MinTemp
#Medidas de centralidad
summary(var_MinTemp)
#medidas de dispersión
medidas_dispersion<-function(x) {
   x1 = x %>% na.omit()
  return(list('rango'= range(x1), 'Varianza'= var(x1), 'Desciación_tipica'= sd(x1)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_MinTemp)

length(var_MinTemp)
# Basic histogram
ggplot(train, aes(x=MinTemp)) + geom_histogram() + ggtitle('Histograma de la Temperatura Mínima')
# Basic Density plot
ggplot(train, aes(x = MinTemp)) + geom_density(fill="white") + ggtitle('Función de densidad de la Temperatura Mínima')
# Basic box plot
ggplot(train, aes(y=MinTemp)) +  geom_boxplot(fill="white") + ggtitle('Boxplot de la Temperatura Mínima')

#MaxTemp
var_MaxTemp = train$MaxTemp
#Medidas de centralidad
summary(var_MinTemp)
#Medidas de dispersión
medidas_dispersion(var_MaxTemp)
# Basic histogram
ggplot(train, aes(x=var_MaxTemp)) + geom_histogram()  + ggtitle('Histograma de la Temperatura Máxima')
# Basic Density plot
ggplot(train, aes(x = MinTemp)) + geom_density(fill= 'white') + ggtitle('Función de densidad de la Temperatura Máxima')
# Basic box plot
ggplot(train, aes(y=var_MaxTemp)) +  geom_boxplot() + ggtitle('Boxplot de la Temperatura Máxima')

#RainFall: The amount of rainfall recorded for the day in mm
var_Rainfall = train$Rainfall
#Medidas de centralidad
summary(var_Rainfall)
#Medidas de dispersión
medidas_dispersion(var_Rainfall)
# Basic histogram
ggplot(train, aes(x=var_Rainfall)) + geom_histogram() + ggtitle('Histograma de los mm de lluvia')
# Basic Density plot
ggplot(train, aes(x = var_Rainfall)) + geom_density() + ggtitle('Función de densidad de los mm de lluvia')
# Basic box plot
ggplot(train, aes(y=var_Rainfall)) +  geom_boxplot() + ggtitle('Boxplot de los mm de lluvia')

#Evaporation: The so-called Class A pan evaporation (mm) in the 24 hours to 9am
var_Evaporation = train$Evaporation
#Medidas de centralidad
summary(var_Evaporation)
#Medidas de dispersión
medidas_dispersion(var_Evaporation)
# Basic histogram
ggplot(train, aes(x=var_Evaporation)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(train, aes(x = var_Evaporation)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(train, aes(y=var_Evaporation)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')
# Basic scatter plot
#ggplot(train, aes(x=Evaporation, y=Location)) + geom_point()

#Cloud 9am
var_Cloud9am = train$Cloud9am
#Medidas de centralidad
summary(var_Cloud9am)
#Medidas de dispersión
medidas_dispersion(var_Cloud9am)
# Basic histogram
ggplot(train, aes(x=var_Cloud9am)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(train, aes(x = var_Cloud9am)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(train, aes(y=var_Cloud9am)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Cloud 3pm
var_Cloud3pm = train$Cloud3pm
#Medidas de centralidad
summary(var_Cloud3pm)
#Medidas de dispersión
medidas_dispersion(var_Cloud3pm)
# Basic histogram
ggplot(train, aes(x=var_Cloud3pm)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(train, aes(x = var_Cloud3pm)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(train, aes(y=var_Cloud3pm)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Temp 9am
var_Temp9am = train$Temp9am
#Medidas de centralidad
summary(var_Temp9am)
#Medidas de dispersión
medidas_dispersion(var_Temp9am)
# Basic histogram
ggplot(train, aes(x=var_Temp9am)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(train, aes(x = var_Temp9am)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(train, aes(y=var_Temp9am)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Temp3pm
var_Temp3pm = train$Temp3pm
#Medidas de centralidad
summary(var_Temp3pm)
#Medidas de dispersión
medidas_dispersion(var_Temp3pm)
# Basic histogram
ggplot(train, aes(x=var_Temp3pm)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(train, aes(x = var_Temp3pm)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(train, aes(y=var_Temp3pm)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

# EDA multivariante
# Diagramas de barras
# Histogramas separando variables
train %>% 
  ggplot(aes(x = MinTemp, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

train %>% 
  ggplot(aes(x = MaxTemp, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

train %>% 
  ggplot(aes(x = Cloud9am, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

train %>% 
  ggplot(aes(x = Cloud3pm, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

train %>% 
  ggplot(aes(x = Temp9am, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

train %>% 
  ggplot(aes(x = Temp3pm, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

# Densidades separando variables
train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MinTemp, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MaxTemp, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Rainfall, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Cloud9am, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Cloud3pm, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Temp9am, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Temp3pm, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

# Boxplot separando variables
ggplot(train, aes(x = RainTomorrow, y = MinTemp, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = MaxTemp, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = Rainfall, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = Cloud9am, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = Cloud3pm, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = Temp9am, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(train, aes(x = RainTomorrow, y = Temp3pm, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')

# Diagrama violín
train %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = RainTomorrow, y = MaxTemp)) +
  geom_violin(aes(fill = test)) + 
  scale_fill_brewer(palette="Paired") +
  ggtitle('')

# Correlaciones
train %>% select(2, 4:7) %>%
  na.omit() %>%
  ggpairs(columns = 2:5, ggplot2::aes(colour=Season))

#Correlación variables numéricas
train_num = train %>% select_if(is.numeric) %>% na.omit()
train_matrix = cor(train_num)
corrplot(train_matrix,is.corr = FALSE, method='circle', order = "hclust", addrect = 2, tl.cex=0.8, tl.col = "black")

#Scatterplot de las variables más correladas:
#MinTemp, Temp9am
ggplot(train, aes(x=MinTemp, y=Temp9am)) + geom_point()
#MaxTemp, Temp3pm
ggplot(train, aes(x=MaxTemp, y=Temp3pm)) + geom_point()
#MaxTemp, Temp3pm
ggplot(train, aes(x=Pressure3pm, y=Pressure9am)) + geom_point()

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

# Visualización de valores faltantes: no parece que estén relacionados.
train %>% select(Sunshine, Evaporation) %>% marginplot()
train %>% select(Cloud3pm, Cloud9am) %>% marginplot()

#Sí parecen relacionados:
train %>% select(Pressure3pm, Pressure9am) %>% marginplot()
train %>% select(Humidity3pm, Humidity9am) %>% marginplot()

#Imputación de valores faltantes
#prueba función
#imputacion_knn_train<-function(df, feature1, feature2) {
#    par(mfrow=c(1,1))
#    df %>% select(feature1, feature2) %>% marginplot()
#    df %>% select(feature1, feature2) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
#    df_imputed <- kNN(train, variable=c(as.character(feature1), as.character(feature2)))
#    aggr(df_imputed%>% select(Temp9am_imp, Temp3pm_imp, feature1, feature2), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
#    par(mfrow=c(1,2))
#    plot(density(df$feature2, na.rm = T), col=2, main= as.character(feature2))
#    lines(density(df_imputed$feature2), col=3)
#    plot(density(df$feature1, na.rm = T), col=2, main=as.character(feature1))
#    lines(density(df_imputed$feature1), col=3)
#    return (df_imputed)}

#train_imputed = imputacion_knn(train, Temp9am, Temp3pm)

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

#Variables Cloud9am & Cloud3pm: buen método?
#par(mfrow=c(1,1))
#train %>% select(Cloud9am, Cloud3pm) %>% marginplot()
#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
#train %>% select(Cloud9am, Cloud3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
#train_imputed3 <- kNN(train, variable=c("Cloud9am","Cloud3pm"))

#ggplot(train, aes(x = Cloud9am)) + geom_density(fill='white') + 
#ggplot(train_imputed3, aes(x = Cloud9am)) + geom_density(fill='white') + ggtitle('')

#Comprobaciones:
#par(mfrow=c(1,2))
#plot(density(train$Cloud3pm,na.rm = T),col=2,main="Cloud3pm")
#lines(density(train_imputed0$Cloud3pm),col=3)
#plot(density(train$Cloud9am,na.rm = T),col=2,main="Cloud9am")
#lines(density(train_imputed0$Cloud9am),col=3)

#Otro método: imputación múltiple 
par(mfrow=c(1,1))
train_imputed0 <- mice(train_imputed[,c('Cloud3pm','Cloud9am')], seed=2018, print = F, m = 30)
train_imputed1 <- mice::complete(train_imputed0)
xyplot(train_imputed0, Cloud3pm ~Cloud9am)

#Pendiente: ver cómo unirlo al dataframe original.

par(mfrow=c(1,2))
plot(density(train$Cloud3pm,na.rm = T),col=2,main="Cloud3pm")
lines(density(train_imputed1$Cloud3pm),col=3)
plot(density(train$Cloud9am,na.rm = T),col=2,main="Cloud9am")
lines(density(train_imputed1$Cloud9am),col=3)

#Final
par(mfrow=c(1,1))
var_Temp = train_imputed %>% select(MinTemp, MaxTemp, Location, Temp9am, Temp3pm, Rainfall)
var_cloud = train_imputed1 %>% select(Cloud9am, Cloud3pm)

aggr_plot <- aggr(var_Temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_Temp), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot

aggr_plot <- aggr(var_cloud, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_cloud), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

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

#Normalización
#preprocessParams <- preProcess(train_imputed[,1:4], method=c("range"))
#transformed <- predict(preprocessParams, train_imputed[,1:4])

#Estandarización
numeric_vars <- c("MinTemp","MaxTemp", "Rainfall", "Evaporation", "Sunshine", "WindGustSpeed", 
                  "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm","Pressure9am" ,
                  "Pressure3pm" , "Temp9am", "Temp3pm", "RISK_MM")

preprocessParams <- preProcess(train_imputed[numeric_vars], method=c("center", "scale"))
train_imputed[numeric_vars] <- predict(preprocessParams, train_imputed[numeric_vars])

#Aplicamos la estandarización con las medias y varianzas de train
test_imputed[numeric_vars] <- predict(preprocessParams, test_imputed[numeric_vars])

#Procesado de variables cualitativas: creación variables dummies
train_imputed <- dummy_cols(train_imputed, select_columns = c("Location", 'Season'))
test_imputed <- dummy_cols(train_imputed, select_columns = c("Location", 'Season'))

train_imputed$Location <- NULL
train_imputed$Season <- NULL
train_imputed$Date <- NULL

test_imputed$Location <- NULL
test_imputed$Season <- NULL
test_imputed$Date <- NULL

#Datasets preparados:
x_train = train_imputed[, !names(train_imputed) %in% c("RainTomorrow")] 
y_train = train_imputed$RainTomorrow

#Selección de variables 

train_imputed_sinNA = na.omit(train_imputed)

#Primero vemos Lasso
library(glmnet)

x = model.matrix(train_imputed ~ ., RainTomorrow)[,-1]

lambda_seq <- 10^seq(2, -2, by = -.1)
cv.out <- cv.glmnet(x, y_train, alpha = 1, lambda = lambda_seq)
plot(cv_output)
# identifying best lamda
best_lam <- cv_output$lambda.min
lasso_best <- glmnet1(x_train, y_train, alpha = 1, lambda = best_lam)

c<-coef(glmnet1,s=best_lam,exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]

#Modelo


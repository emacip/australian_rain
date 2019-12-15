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

weatherAUS <- read.csv("~/Escritorio/MASTER/FUNDAMENTOS DE ANÁLISIS DE DATOS/practica_FAD/australian_rain/weatherAUS_2.csv")

dim(weatherAUS)
colors_rain<-c('blue', 'purple')

#Estudio variables
#Location
var_location = weatherAUS$Location
length(levels(var_location))
ggplot(data=weatherAUS, aes(x=Location, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))
#comparación con el target
ggplot(weatherAUS, aes(Location)) + geom_bar(aes(stat="identity", fill = RainTomorrow), position = position_dodge(0.9)) + theme(axis.text.x=element_text(angle=90,hjust=1)) + scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4"))

#tabla de contingencia
with(weatherAUS, table(Location, RainTomorrow))
with(weatherAUS, table(Location, RainToday))

#MinTemp
var_MinTemp = weatherAUS$MinTemp
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
ggplot(weatherAUS, aes(x=MinTemp)) + geom_histogram() + ggtitle('Histograma de la Temperatura Mínima')
# Basic Density plot
ggplot(weatherAUS, aes(x = MinTemp)) + geom_density(fill="white") + ggtitle('Función de densidad de la Temperatura Mínima')
# Basic box plot
ggplot(weatherAUS, aes(y=MinTemp)) +  geom_boxplot(fill="white") + ggtitle('Boxplot de la Temperatura Mínima')

#MaxTemp
var_MaxTemp = weatherAUS$MaxTemp
#Medidas de centralidad
summary(var_MinTemp)
#Medidas de dispersión
medidas_dispersion(var_MaxTemp)
# Basic histogram
ggplot(weatherAUS, aes(x=var_MaxTemp)) + geom_histogram()  + ggtitle('Histograma de la Temperatura Máxima')
# Basic Density plot
ggplot(weatherAUS, aes(x = MinTemp)) + geom_density(fill= 'white') + ggtitle('Función de densidad de la Temperatura Máxima')
# Basic box plot
ggplot(weatherAUS, aes(y=var_MaxTemp)) +  geom_boxplot() + ggtitle('Boxplot de la Temperatura Máxima')

#RainFall: The amount of rainfall recorded for the day in mm
var_Rainfall = weatherAUS$Rainfall
#Medidas de centralidad
summary(var_Rainfall)
#Medidas de dispersión
medidas_dispersion(var_Rainfall)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Rainfall)) + geom_histogram() + ggtitle('Histograma de los mm de lluvia')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Rainfall)) + geom_density() + ggtitle('Función de densidad de los mm de lluvia')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Rainfall)) +  geom_boxplot() + ggtitle('Boxplot de los mm de lluvia')

#Evaporation: The so-called Class A pan evaporation (mm) in the 24 hours to 9am
var_Evaporation = weatherAUS$Evaporation
#Medidas de centralidad
summary(var_Evaporation)
#Medidas de dispersión
medidas_dispersion(var_Evaporation)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Evaporation)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Evaporation)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Evaporation)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')
# Basic scatter plot
#ggplot(weatherAUS, aes(x=Evaporation, y=Location)) + geom_point()

#Cloud 9am
var_Cloud9am = weatherAUS$Cloud9am
#Medidas de centralidad
summary(var_Cloud9am)
#Medidas de dispersión
medidas_dispersion(var_Cloud9am)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Cloud9am)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Cloud9am)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Cloud9am)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Cloud 3pm
var_Cloud3pm = weatherAUS$Cloud3pm
#Medidas de centralidad
summary(var_Cloud3pm)
#Medidas de dispersión
medidas_dispersion(var_Cloud3pm)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Cloud3pm)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Cloud3pm)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Cloud3pm)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Temp 9am
var_Temp9am = weatherAUS$Temp9am
#Medidas de centralidad
summary(var_Temp9am)
#Medidas de dispersión
medidas_dispersion(var_Temp9am)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Temp9am)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Temp9am)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Temp9am)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

#Temp3pm
var_Temp3pm = weatherAUS$Temp3pm
#Medidas de centralidad
summary(var_Temp3pm)
#Medidas de dispersión
medidas_dispersion(var_Temp3pm)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Temp3pm)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Temp3pm)) + geom_density(fill='white') + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Temp3pm)) +  geom_boxplot(fill='white') + ggtitle('Boxplot de los mm de evaporación')

# EDA multivariante
# Diagramas de barras
# Histogramas separando variables
weatherAUS %>% 
  ggplot(aes(x = MinTemp, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

weatherAUS %>% 
  ggplot(aes(x = MaxTemp, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

weatherAUS %>% 
  ggplot(aes(x = Cloud9am, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

weatherAUS %>% 
  ggplot(aes(x = Cloud3pm, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

weatherAUS %>% 
  ggplot(aes(x = Temp9am, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

weatherAUS %>% 
  ggplot(aes(x = Temp3pm, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

# Densidades separando variables
weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MinTemp, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MaxTemp, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Rainfall, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Cloud9am, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Cloud3pm, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Temp9am, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Temp3pm, fill = test)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades MinTemp en función de RainTomorrow')

# Boxplot separando variables
ggplot(weatherAUS, aes(x = RainTomorrow, y = MinTemp, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = MaxTemp, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = Rainfall, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = Cloud9am, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = Cloud3pm, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = Temp9am, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')
ggplot(weatherAUS, aes(x = RainTomorrow, y = Temp3pm, fill = RainTomorrow)) + geom_boxplot() +  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +  ggtitle(' ')

# Diagrama violín
weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = RainTomorrow, y = MaxTemp)) +
  geom_violin(aes(fill = test)) + 
  scale_fill_brewer(palette="Paired") +
  ggtitle('')

# Correlaciones
weatherAUS %>% select(2, 4:7) %>%
  na.omit() %>%
  ggpairs(columns = 2:5, ggplot2::aes(colour=Season))

#Correlación variables numéricas
weatherAUS_num = weatherAUS %>% select_if(is.numeric) %>% na.omit()
weatherAUS_matrix = cor(weatherAUS_num)
corrplot(weatherAUS_matrix,is.corr = FALSE, method='circle', order = "hclust", addrect = 2, tl.cex=0.8, tl.col = "black")

#Scatterplot de las variables más correladas:
#MinTemp, Temp9am
ggplot(weatherAUS, aes(x=MinTemp, y=Temp9am)) + geom_point()
#MaxTemp, Temp3pm
ggplot(weatherAUS, aes(x=MaxTemp, y=Temp3pm)) + geom_point()
#MaxTemp, Temp3pm
ggplot(weatherAUS, aes(x=Pressure3pm, y=Pressure9am)) + geom_point()

#Detección, tratamiento e imputación de datos faltantes
#Valores Faltantes
# Visualización de valores faltantes
#Variables con más valores faltantes: Sunshine, Evaporation, Cloud3pm, Cloud9am, Pressure3pm, Pressure9am
var_Temp = weatherAUS %>% select(MinTemp, MaxTemp, Location, Temp9am, Temp3pm)
var_cloud = weatherAUS %>% select(Cloud9am, Cloud3pm, Temp9am, Temp3pm)

aggr_plot <- aggr(var_Temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_Temp), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot

aggr_plot <- aggr(var_cloud, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_cloud), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# Visualización de valores faltantes: no parece que estén relacionados.
weatherAUS %>% select(Sunshine, Evaporation) %>% marginplot()
weatherAUS %>% select(Cloud3pm, Cloud9am) %>% marginplot()

#Sí parecen relacionados:
weatherAUS %>% select(Pressure3pm, Pressure9am) %>% marginplot()
weatherAUS %>% select(Humidity3pm, Humidity9am) %>% marginplot()

#Imputación de valores faltantes
#Variables Temp9am & Temp3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
weatherAUS %>% select(Temp9am, Temp3pm) %>% marginplot()
#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
#Parece que tiene sentido:
weatherAUS %>% select(Temp9am, Temp3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
weatherAUS_imputed <- kNN(weatherAUS, variable=c("Temp9am","Temp3pm"))
aggr(weatherAUS_imputed%>% select(Temp9am_imp, Temp3pm_imp, Temp9am, Temp3pm), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(weatherAUS$Temp9am,na.rm = T),col=2,main="Temp9am")
lines(density(weatherAUS_imputed$Temp9am),col=3)
plot(density(weatherAUS$Temp3pm,na.rm = T),col=2,main="Temp3pm")
lines(density(weatherAUS_imputed$Temp3pm),col=3)

#Variables MinTemp & MaxTemp: método: imputación simple: KNN
par(mfrow=c(1,1))
weatherAUS %>% select(MinTemp, MaxTemp) %>% marginplot()
#Parece que tiene sentido:
weatherAUS_imputed %>% select(MinTemp, MaxTemp) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
weatherAUS_imputed <- kNN(weatherAUS_imputed, variable=c("MaxTemp","MinTemp"))
aggr(weatherAUS_imputed%>% select(MinTemp, MaxTemp, MaxTemp_imp, MinTemp_imp), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(weatherAUS$MaxTemp,na.rm = T),col=2,main="MaxTemp")
lines(density(weatherAUS_imputed$MaxTemp),col=3)
plot(density(weatherAUS$MinTemp,na.rm = T),col=2,main="MinTemp")
lines(density(weatherAUS_imputed$MinTemp),col=3)

#Variables Humidity9am & Rainfall: método: imputación simple: KNN
par(mfrow=c(1,1))
weatherAUS %>% select(Humidity9am, Rainfall) %>% marginplot()
#Parece que tiene sentido:
weatherAUS %>% select(Humidity9am, Rainfall) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
weatherAUS_imputed <- kNN(weatherAUS_imputed, variable=c("Humidity9am","Rainfall"))
aggr(weatherAUS_imputed%>% select(Humidity9am, Rainfall, Humidity9am_imp, Rainfall_imp), delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(weatherAUS$Humidity9am,na.rm = T),col=2,main="Humidity9am")
lines(density(weatherAUS_imputed$Humidity9am),col=3)
plot(density(weatherAUS$Rainfall,na.rm = T),col=2,main="Rainfall")
lines(density(weatherAUS_imputed$Rainfall),col=3)

#Variables Cloud9am & Cloud3pm: buen método?
par(mfrow=c(1,1))
weatherAUS %>% select(Cloud9am, Cloud3pm) %>% marginplot()
#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
weatherAUS %>% select(Cloud9am, Cloud3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
weatherAUS_imputed <- kNN(weatherAUS, variable=c("Cloud9am","Cloud3pm"))

ggplot(weatherAUS, aes(x = Cloud9am)) + geom_density(fill='white') + 
ggplot(weatherAUS_imputed, aes(x = Cloud9am)) + geom_density(fill='white') + ggtitle('')

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(weatherAUS$Cloud3pm,na.rm = T),col=2,main="Cloud3pm")
lines(density(weatherAUS_imputed$Cloud3pm),col=3)
plot(density(weatherAUS$Cloud9am,na.rm = T),col=2,main="Cloud9am")
lines(density(weatherAUS_imputed$Cloud9am),col=3)

#Otro método: imputación múltiple
par(mfrow=c(1,1))
weatherAUS_imputed0 <- mice(weatherAUS_imputed[,c('Cloud3pm','Cloud9am')], seed=2018,print = F, m = 30)
weatherAUS_imputed1 <- mice::complete(weatherAUS_imputed0)
xyplot(weatherAUS_imputed0, Cloud3pm ~Cloud9am)

par(mfrow=c(1,2))
plot(density(weatherAUS$Cloud3pm,na.rm = T),col=2,main="Cloud3pm")
lines(density(weatherAUS_imputed1$Cloud3pm),col=3)
plot(density(weatherAUS$Cloud9am,na.rm = T),col=2,main="Cloud9am")
lines(density(weatherAUS_imputed1$Cloud9am),col=3)

#Final
par(mfrow=c(1,1))
var_Temp = weatherAUS_imputed %>% select(MinTemp, MaxTemp, Location, Temp9am, Temp3pm)
var_cloud = weatherAUS_imputed1 %>% select(Cloud9am, Cloud3pm)

aggr_plot <- aggr(var_Temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_Temp), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot

aggr_plot <- aggr(var_cloud, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(var_cloud), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

#Transformaciones de variables cuantitativas
#La única que no parece seguir una distribución normal es la variable RainFall
#No se le puede aplicar logaritmos porque hay nulos...
p1 <- weatherAUS %>% select(Rainfall, Season) %>%
  na.omit() %>%
  ggplot(aes(x=Rainfall, colour=Season)) +
  geom_density()

p2 <- weatherAUS %>% mutate(log10_Rainfall = log10(Rainfall)) %>%
  select(Season, log10_Rainfall) %>%
  na.omit() %>%
  ggplot(aes(x=log10_Rainfall, colour=Season)) +
  geom_density()

grid.arrange(p1, p2, nrow = 1)

p2 <- weatherAUS %>% mutate(log10_Rainfall = log10(Rainfall))
qqnorm(p2$log10_Rainfall, ylab=" ")
qqline(p2$log10_Rainfall, col="red")

#Resto de variables: siguen distribución normal
qqnorm(weatherAUS_imputed$MinTemp, ylab="MinTemp")
qqline(weatherAUS_imputed$MinTemp, col="red")

qqnorm(weatherAUS_imputed$MaxTemp, ylab="MaxTemp")
qqline(weatherAUS_imputed$MaxTemp, col="red")

qqnorm(weatherAUS_imputed$Temp3pm, ylab="Temp3pm")
qqline(weatherAUS_imputed$Temp3pm, col="red")

qqnorm(weatherAUS_imputed$Temp9am, ylab="Temp9am")
qqline(weatherAUS_imputed$Temp9am, col="red")

#Procesado de variables cualitativas

#Selección de variables 


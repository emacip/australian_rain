  #https://www.kaggle.com/jsphyg/weather-dataset-rattle-package
#Lectura de los datos
library(dplyr)
library(tidyverse)
library(ggplot2)
library(VIM)
weatherAUS <- read.csv("~/Escritorio/MASTER/FUNDAMENTOS DE ANÁLISIS DE DATOS/practica_FAD/weatherAUS.csv")

dim(weatherAUS)

#Estudio variables
#Location
var_location = weatherAUS$Location
length(levels(var_location))
ggplot(data=weatherAUS, aes(x=Location, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))
#comparación con el target
ggplot(weatherAUS, aes(Location, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#tabla de contingencia
with(weatherAUS, table(Location, RainTomorrow))
with(weatherAUS, table(Location, RainToday))

#MinTemp
var_MinTemp = weatherAUS$MinTemp
#Medidas de centralidad
summary(var_MinTemp)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_MinTemp)

length(var_MinTemp)
# Basic histogram
ggplot(weatherAUS, aes(x=MinTemp)) + geom_histogram() + ggtitle('Histograma de la Temperatura Mínima')
# Basic Density plot
ggplot(weatherAUS, aes(x = MinTemp)) + geom_density() + ggtitle('Función de densidad de la Temperatura Mínima')
# Basic box plot
ggplot(weatherAUS, aes(y=MinTemp)) +  geom_boxplot() + ggtitle('Boxplot de la Temperatura Mínima')
# Basic scatter plot
ggplot(weatherAUS, aes(x=MinTemp, y=Location)) + geom_point() 


#MaxTemp
var_MaxTemp = weatherAUS$MaxTemp
#Medidas de centralidad
summary(var_MinTemp)
#Medidas de dispersión
medidas_dispersion(var_MaxTemp)
# Basic histogram
ggplot(weatherAUS, aes(x=var_MaxTemp)) + geom_histogram()  + ggtitle('Histograma de la Temperatura Máxima')
# Basic Density plot
ggplot(weatherAUS, aes(x = MinTemp)) + geom_density() + ggtitle('Función de densidad de la Temperatura Máxima')
# Basic box plot
ggplot(weatherAUS, aes(y=var_MaxTemp)) +  geom_boxplot() + ggtitle('Boxplot de la Temperatura Máxima')
# Basic scatter plot
ggplot(weatherAUS, aes(x=MaxTemp, y=Location)) + geom_point()


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
# Basic scatter plot
ggplot(weatherAUS, aes(x=var_Rainfall, y=Location)) + geom_point()

#Evaporation: The so-called Class A pan evaporation (mm) in the 24 hours to 9am
var_Evaporation = weatherAUS$Evaporation
#Medidas de centralidad
summary(var_Evaporation)
#Medidas de dispersión
medidas_dispersion(var_Evaporation)
# Basic histogram
ggplot(weatherAUS, aes(x=var_Evaporation)) + geom_histogram() + ggtitle('Histograma de los mm de evaporación')
# Basic Density plot
ggplot(weatherAUS, aes(x = var_Evaporation)) + geom_density() + ggtitle('Función de densidad de los mm de evaporación')
# Basic box plot
ggplot(weatherAUS, aes(y=var_Evaporation)) +  geom_boxplot() + ggtitle('Boxplot de los mm de evaporación')
# Basic scatter plot
ggplot(weatherAUS, aes(x=Evaporation, y=Location)) + geom_point()

# EDA multivariante

# Dotplot
## Cleveland dotplot (variable cuantitativa frente a categórica)
## Media de activos por sector para todos los países
weatherAUS %>%
  group_by(Location) %>%
  summarize(avg_Rainfall = mean(Rainfall)) %>%
  ggplot(aes(x = avg_Rainfall, y = reorder(Location, avg_Rainfall))) + 
  geom_point(size = 5)

# Diagramas de barras

# Histogramas separando variables
weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MinTemp)) +
  geom_histogram(fill="white", colour="black") +
  facet_wrap(~ test)

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MaxTemp)) +
  geom_histogram(fill="white", colour="black") +
  facet_wrap(~ test)

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Evaporation)) +
  geom_histogram(fill="white", colour="black") +
  facet_wrap(~ test)

# Densidades separando variables

weatherAUS %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = MinTemp, fill = test)) +
  geom_density(alpha = .3) +
  ggtitle(' ')

# Boxplot separando variables
ggplot(weatherAUS, aes(x = RainTomorrow, y = MinTemp)) + geom_boxplot() +  ggtitle(' ')

# Diagrama violín


#Detección, tratamiento e imputación de datos faltantes
#Valores Faltantes
# Visualización de valores faltantes
#Variables con más valores faltantes: Sunshine, Evaporation, Cloud3pm, Cloud9am, Pressure3pm, Pressure9am
aggr_plot <- aggr(weatherAUS, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(weatherAUS), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot

# Visualización de valores faltantes: no parece que estén relacionados.
weatherAUS %>% select(Sunshine, Evaporation) %>% marginplot()
weatherAUS %>% select(Cloud3pm, Cloud9am) %>% marginplot()

#Sí parecen relacionados:
weatherAUS %>% select(Pressure3pm, Pressure9am) %>% marginplot()
weatherAUS %>% select(Humidity3pm, Temp3pm) %>% marginplot()
#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN

#Imputación de valores faltantes
library(mice)
weatherAUS_imputed = mice(weatherAUS, m = 3, maxit = 10, method = "pmm", seed = 300, printFlag = FALSE)
summary(weatherAUS_imputed)

#Variables numéricas
weatherAUS %>% select_if(is.numeric) %>% na.omit()
dim(weatherAUS %>% select_if(is.numeric) %>% na.omit())

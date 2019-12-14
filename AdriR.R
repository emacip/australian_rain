library(dplyr)
library(tidyr)
library(ggplot2)
library(VIM)
library(readr)
library(GGally)
library()



weatherAUS <- read_csv("weatherAUS.csv")
weatherAUS <- filter(weatherAUS, Location == "Sydney")
weatherAUS_WindDir9am <- select(weatherAUS,WindDir9am)
weatherAUS_WindDir9am

weatherAUS %>% select(Cloud9am,Cloud3pm,Temp9am,Temp3pm,RainToday,RainTomorrow) %>%
  na.omit() %>%
  ggpairs(columns = 1:6, ggplot2::aes(colour=RainTomorrow))

weatherAUS %>% select(Sunshine,Humidity9am,Humidity3pm,Pressure9am,Pressure3pm,RainTomorrow) %>%
  na.omit() %>%
  ggpairs(columns = 1:6, ggplot2::aes(colour=RainTomorrow))

weatherAUS %>% select(Evaporation,WindDir9am,WindDir3pm,WindSpeed9am,WindSpeed3pm,RainTomorrow) %>%
  na.omit() %>%
  ggpairs(columns = 1:6, ggplot2::aes(colour=RainTomorrow))

weatherAUS %>% select(MinTemp,MaxTemp,Rainfall,WindGustSpeed,RainTomorrow) %>%
  na.omit() %>%
  ggpairs(columns = 1:5, ggplot2::aes(colour=RainTomorrow))


## (categórica)   WindDir9am, WindDir3pm, 
## (cuantitativa) WindSpeed9am, WindSpeed3pm,  Humidity9am, Humidity3pm, Pressure9am, Pressure3pm


## Variables categoricas (politómicas)

default.stringsAsFactors()

## WindDir9am

var_WindDir9am = weatherAUS$WindDir9am
length(levels(var_WindDir9am))
ggplot(data=weatherAUS, aes(x=WindDir9am, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))

#comparo con raintomorrow

ggplot(weatherAUS, aes(WindDir9am, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#factorizamos aqui simplemente las categorizo luego hay que hacer un analisis con las variables dummy 
weatherAUSnuevo = weatherAUS

weatherAUSnuevo$WindDir9am = factor(weatherAUS$WindDir9am)

## WindDir3pm

var_WindDir3pm = weatherAUS$WindDir3pm
length(levels(var_WindDir3pm))
ggplot(data=weatherAUS, aes(x=WindDir3pm, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))

#comparo con raintomorrow

ggplot(weatherAUS, aes(WindDir3pm, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#factorizamos aqui simplemente las categorizo luego hay que hacer un analisis con las variables dummy 
weatherAUSnuevo = weatherAUS

weatherAUSnuevo$WindDir3pm = factor(weatherAUS$WindDir3pm)

## Variables cuantitativa

## WindSpeed9am

var_WindSpeed9am = weatherAUS$WindSpeed9am
#Medidas de centralidad
summary(var_WindSpeed9am)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_WindSpeed9am)

length(var_WindSpeed9am)
# Basic histogram
ggplot(weatherAUS, aes(x=WindSpeed9am)) + geom_histogram() + ggtitle('Histograma de WindSpeed9am')
# Basic Density plot
ggplot(weatherAUS, aes(x = WindSpeed9am)) + geom_density() + ggtitle('Función de densidad de WindSpeed9am')
# Basic box plot
ggplot(weatherAUS, aes(y=WindSpeed9am)) +  geom_boxplot() + ggtitle('Boxplot de WindSpeed9am')
# Basic scatter plot
ggplot(weatherAUS, aes(x=WindSpeed9am, y=RainTomorrow)) + geom_point() 

#Estudio variables
#Location
var_location = train$Location
length(levels(var_location))
ggplot(data=train, aes(x=Location, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))
#comparación con el target
ggplot(train, aes(Location)) + geom_bar(aes(stat="identity", fill = RainTomorrow), position = position_dodge(0.9)) + theme(axis.text.x=element_text(angle=90,hjust=1)) + scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4"))


#MinTemp
var_MinTemp = train$MinTemp
#Medidas de centralidad
summary(var_MinTemp)

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






#TODO poner misma dataset
data <- train
# Evaporation
evap <- data$Evaporation
summary(evap)
medidas_dispersion(evap)
hist(evap)
# missing data
aggr(evap, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(evap), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# boxplot
ggplot(data, aes(y=evap)) +  geom_boxplot() 
# scatter plot
ggplot(data, aes(x=data$RainTomorrow, y=evap)) + geom_point()
# margin plot
data %>% select(Evaporation, Sunshine) %>% marginplot()


# Sunshine
sunshine <- data$Sunshine
summary(sunshine)
medidas_dispersion(sunshine)
hist(sunshine)
# missing data
aggr(sunshine, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(sunshine), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# boxplot
ggplot(weatherAUS_2, aes(y=sunshine)) +  geom_boxplot() 
# scatter plot
ggplot(weatherAUS_2, aes(x=data$RainTomorrow, y=sunshine)) + geom_point()


# WindGustDir
wind_gust_dir <- data$WindGustDir
summary(wind_gust_dir)
# missing data
aggr(wind_gust_dir, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(wind_gust_dir), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# histogram
ggplot(data, aes(x = wind_gust_dir)) + geom_bar()


# WindGustSpeed
wind_gust_speed <- data$WindGustSpeed
summary(wind_gust_speed)
hist(wind_gust_speed)
# missing data
aggr(wind_gust_speed, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(wind_gust_speed), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# boxplot
ggplot(weatherAUS_2, aes(y=wind_gust_speed)) +  geom_boxplot() 
# scatter plot
ggplot(weatherAUS_2, aes(x=data$WindGustDir, y=wind_gust_speed)) + geom_point()


# RainToday
rain_today <- data$RainToday
summary(rain_today)
# missing data
aggr(rain_today, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(rain_today), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# histogram
ggplot(data, aes(x = rain_today)) + geom_bar()


# RISK_MM 
# The amount of next day rain in mm. Used to create response variable RainTomorrow. 
# How much rain recorded in millimetres. 
risk_mm <- data$RISK_MM
summary(risk_mm)
hist(risk_mm)
# missing data
aggr(risk_mm, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(risk_mm), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# boxplot
ggplot(data, aes(y=risk_mm)) +  geom_boxplot() 
# scatter plot
ggplot(data, aes(x=data$RainTomorrow, y=risk_mm)) + geom_point()


# RainTomorrow
rain_tomorrow <- data$RainTomorrow
summary(rain_tomorrow)
# missing data
aggr(rain_tomorrow, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(rain_tomorrow), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# histogram
ggplot(data, aes(x = rain_tomorrow)) + geom_bar()


## Variables categoricas (politómicas)

default.stringsAsFactors()

## WindDir9am

var_WindDir9am = train$WindDir9am
length(levels(var_WindDir9am))
ggplot(data=train, aes(x=WindDir9am, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))

#comparo con raintomorrow

with(train, table(WindDir9am, RainTomorrow))
(xtabs(~ RainTomorrow + WindDir9am, data = train))


ggplot(train, aes(WindDir9am, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#factorizamos aqui simplemente las categorizo luego hay que hacer un analisis con las variables dummy 
weatherAUSnuevo = train

weatherAUSnuevo$WindDir9am = factor(train$WindDir9am)

## WindDir3pm

var_WindDir3pm = train$WindDir3pm
length(levels(var_WindDir3pm))
ggplot(data=train, aes(x=WindDir3pm, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))

#comparo con raintomorrow

with(train, table(WindDir3pm, RainTomorrow))
ftable(xtabs(~ RainTomorrow + WindDir3pm, data = train))



ggplot(train, aes(WindDir3pm, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#factorizamos aqui simplemente las categorizo luego hay que hacer un analisis con las variables dummy 
weatherAUSnuevo = train

weatherAUSnuevo$WindDir3pm = factor(train$WindDir3pm)

## Variables cuantitativa

## WindSpeed9am

var_WindSpeed9am = train$WindSpeed9am
#Medidas de centralidad
summary(var_WindSpeed9am)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_WindSpeed9am)

length(var_WindSpeed9am)
# Basic histogram
ggplot(train, aes(x=WindSpeed9am)) + geom_histogram() + ggtitle('Histograma de WindSpeed9am')
# Basic Density plot
ggplot(train, aes(x = WindSpeed9am)) + geom_density() + ggtitle('Función de densidad de WindSpeed9am')
# Basic box plot
ggplot(train, aes(y=WindSpeed9am)) +  geom_boxplot() + ggtitle('Boxplot de WindSpeed9am')
# Basic scatter plot
ggplot(train, aes(x=WindSpeed9am, y=RainTomorrow)) + geom_point() 


ggplot(train, aes(x = WindSpeed9am, fill = RainTomorrow)) + 
  geom_dotplot( stackgroups = TRUE, binpositions="all")

## WindSpeed3pm

var_WindSpeed3pm = train$WindSpeed3pm
#Medidas de centralidad
summary(var_WindSpeed3pm)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_WindSpeed3pm)

length(var_WindSpeed3pm)
# Basic histogram
ggplot(train, aes(x=WindSpeed3pm)) + geom_histogram() + ggtitle('Histograma de WindSpeed3pm')
# Basic Density plot
ggplot(train, aes(x = WindSpeed3pm)) + geom_density() + ggtitle('Función de densidad de WindSpeed3pm')
# Basic box plot
ggplot(train, aes(y=WindSpeed3pm)) +  geom_boxplot() + ggtitle('Boxplot de WindSpeed3pm')
# Basic scatter plot
ggplot(train, aes(x=WindSpeed3pm, y=RainTomorrow)) + geom_point() 

## Humidity9am

var_Humidity9am = train$Humidity9am
#Medidas de centralidad
summary(var_Humidity9am)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Humidity9am)

length(var_Humidity9am)
# Basic histogram
ggplot(train, aes(x=Humidity9am)) + geom_histogram() + ggtitle('Histograma de Humidity9am')
# Basic Density plot
ggplot(train, aes(x = Humidity9am)) + geom_density() + ggtitle('Función de densidad de Humidity9am')
# Basic box plot
ggplot(train, aes(y=Humidity9am)) +  geom_boxplot() + ggtitle('Boxplot de Humidity9am')
# Basic scatter plot
ggplot(train, aes(x=Humidity9am, y=RainTomorrow)) + geom_point() 

## Humidity3pm

var_Humidity3pm = train$Humidity3pm
#Medidas de centralidad
summary(var_Humidity3pm)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Humidity3pm)

length(var_Humidity3pm)
# Basic histogram
ggplot(train, aes(x=Humidity3pm)) + geom_histogram() + ggtitle('Histograma de Humidity3pm')
# Basic Density plot
ggplot(train, aes(x = Humidity3pm)) + geom_density() + ggtitle('Función de densidad de Humidity3pm')
# Basic box plot
ggplot(train, aes(y=Humidity3pm)) +  geom_boxplot() + ggtitle('Boxplot de Humidity3pm')
# Basic scatter plot
ggplot(train, aes(x=Humidity3pm, y=RainTomorrow)) + geom_point() 

## Pressure9am

var_Pressure9am = train$Pressure9am
#Medidas de centralidad
summary(var_Pressure9am)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Pressure9am)

length(var_Pressure9am)
# Basic histogram
ggplot(train, aes(x=Pressure9am)) + geom_histogram() + ggtitle('Histograma de Pressure9am')
# Basic Density plot
ggplot(train, aes(x = Pressure9am)) + geom_density() + ggtitle('Función de densidad de Pressure9am')
# Basic box plot
ggplot(train, aes(y=Pressure9am)) +  geom_boxplot() + ggtitle('Boxplot de Pressure9am')
# Basic scatter plot
ggplot(train, aes(x=Pressure9am, y=RainTomorrow)) + geom_point() 

## Pressure3pm

var_Pressure3pm = weatherAUS$Pressure3pm
#Medidas de centralidad
summary(var_Pressure3pm)

#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Pressure3pm)

length(var_Pressure3pm)
# Basic histogram
ggplot(train, aes(x=Pressure3pm)) + geom_histogram() + ggtitle('Histograma de Pressure3pm')
# Basic Density plot
ggplot(train, aes(x = Pressure3pm)) + geom_density() + ggtitle('Función de densidad de Pressure3pm')
# Basic box plot
ggplot(train, aes(y=Pressure3pm)) +  geom_boxplot() + ggtitle('Boxplot de Pressure3pm')
# Basic scatter plot
ggplot(train, aes(x=Pressure3pm, y=RainTomorrow)) + geom_point() 




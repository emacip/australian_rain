library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(VIM)
library(visdat)
library(naniar)
library(caTools)
library(corrplot)

#medidas de dispersión
medidas_dispersion <- function(x) {
  return(list('Rango'= range(x), 'Varianza'= var(x), 'Desviación_tipica'= sd(x)))}



#Split data into training and validation sets
set.seed(123)

split = sample.split(weatherAUS_2$RainTomorrow, SplitRatio = 0.80)
training_set = subset(weatherAUS_2, split == TRUE)
test_set = subset(weatherAUS_2, split == FALSE)
dim(training_set)
dim(test_set)
colnames(training_set)

#Estudio variables
# Proportion of missing values
data = training_set %>% select(Evaporation, Sunshine, WindGustDir, WindGustSpeed, RainToday, RISK_MM, RainTomorrow)
vis_dat(data)
vis_miss(train, warn_large_data = FALSE)
(cols_withNa <- apply(data, 2, function(x) sum(is.na(x))))
gg_miss_upset(train)






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



# EDA multivariante
# Diagramas de barras
# Histogramas separando variables
training_set %>% 
  ggplot(aes(x = Evaporation, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

training_set %>% 
  ggplot(aes(x = Sunshine, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

training_set %>% 
  ggplot(aes(x = WindGustSpeed, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

training_set %>% 
  ggplot(aes(x = RISK_MM, fill = RainTomorrow)) + 
  geom_histogram() +
  scale_fill_manual('RainTomorrow', values = c("No" = "lightsteelblue", "Yes" = "deepskyblue4")) +
  facet_wrap(~ RainTomorrow) 

# Densidades separando variables
training_set %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Evaporation, fill = RainTomorrow)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades Evaporation en función de RainTomorrow')

training_set %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = Sunshine, fill = RainTomorrow)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades Sunshine en función de RainTomorrow')

training_set %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = WindGustSpeed, fill = RainTomorrow)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades WindGustSpeed en función de RainTomorrow')

training_set %>%
  mutate(test = factor(RainTomorrow, labels = c('negative', 'positive'))) %>%
  ggplot(aes(x = RISK_MM, fill = RainTomorrow)) + 
  geom_density(alpha = .3) +
  ggtitle('Densidades RISK_MM en función de RainTomorrow')


#Scatterplot de las variables más correladas:
# check relations
dataNAO <- na.omit(data)
numeric <- map_lgl(dataNAO, is.numeric)

correlations <- cor(dataNAO[,numeric])
corrplot(correlations, method = "number")
# no tienen correlacion entre si
dataNAO <- na.omit(training_set)
numeric <- map_lgl(dataNAO, is.numeric)

correlations <- cor(dataNAO[,numeric])
corrplot(correlations, method = "circle")

#Evaporation, MinTemp
ggplot(training_set, aes(x=Evaporation, y=MinTemp)) + geom_point() + geom_smooth(method = "lm")
#Evaporation, MaxTemp
ggplot(training_set, aes(x=Evaporation, y=MaxTemp)) + geom_point() + geom_smooth(method = "lm")
#Evaporation, Temp9am
ggplot(training_set, aes(x=Evaporation, y=Temp9am)) + geom_point() + geom_smooth(method = "lm")
#Evaporation, Temp3pm
ggplot(training_set, aes(x=Evaporation, y=Temp3pm)) + geom_point() + geom_smooth(method = "lm")
#Sunshine, MaxTemp
ggplot(training_set, aes(x=Sunshine, y=MaxTemp)) + geom_point() + geom_smooth(method = "lm")
#Sunshine, Temp3pm
ggplot(training_set, aes(x=Sunshine, y=Temp3pm)) + geom_point() + geom_smooth(method = "lm") 
#WindGustSpeed, WindSeep9am
ggplot(training_set, aes(x=WindGustSpeed, y=WindSpeed9am)) + geom_point()
#WindGustSpeed, WindSeep3pm
ggplot(training_set, aes(x=WindGustSpeed, y=WindSpeed3pm)) + geom_point()



#Imputación de valores faltantes
(cols_withNa <- apply(train, 2, function(x) sum(is.na(x))))
#Converting categorical nominal values "Yes" and "No" to "0" and "1"
rain_today
rain_today[is.na(rain_today)] <- "No"

rain_today <- ifelse(rain_today == "Yes", 1, 0)
data$RainToday = factor(rain_today)

rain_tomorrow <- ifelse(rain_tomorrow == "Yes", 1, 0)
data$RainTomorrow = factor(rain_tomorrow)


# Evaporation
data$Evaporation[which(is.na(data$Evaporation))] <- mean(data$Evaporation,na.rm = TRUE)
hist(log(data$Evaporation))

#Imputacion WindGustSpeed
data$WindGustSpeed[which(is.na(data$WindGustSpeed))] <- mean(data$WindGustSpeed,na.rm = TRUE)

# TODO se podria hacer un for con la media de la velocidad a las 9 y a las 3 ???
a <- data.frame(WindSpeed9am= training_set$WindSpeed9am, WindSpeed3pm= training_set$WindSpeed3pm)
data$WindGustSpeed[which(is.na(data$WindGustSpeed))] <- rowMeans(a, na.rm=TRUE)

# Transformacion WindGustDir
set.seed(1)
getmode <- function(x){
  Md <- unique(x)
  Md[which.max(tabulate(match(x, Md)))]
}

data$WindGustDir[is.na(data$WindGustDir)] <- getmode(data$WindGustDir)
getmode(data$WindGustDir)
data$WindGustDir<-as.numeric(data$WindGustDir)




data %>%
  group_by(RainTomorrow) %>%
  summarise_each(list(~ sum(is.na(.)) / length(.) * 100)) %>%
  t()

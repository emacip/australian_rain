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
library(glmnet)
library(blorr)


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


#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}






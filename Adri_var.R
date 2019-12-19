library(dplyr)
library(tidyr)
library(ggplot2)
library(VIM)
library(readr)
library(GGally)
library(corrplot)

library(lattice)
library(dplyr)
library(VIM)
library(mice)

library(DMwR2)

library(knitr)
library(kableExtra)
library(htmltools)
library(bsplus)
library(RColorBrewer)



weatherAUS <- read_csv("weatherAUS_2.csv")

summary(weatherAUS)


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

with(weatherAUS, table(WindDir9am, RainTomorrow))
(xtabs(~ RainTomorrow + WindDir9am, data = weatherAUS))


ggplot(weatherAUS, aes(WindDir9am, ..count..)) + geom_bar(aes(fill = RainTomorrow),position = "dodge")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#factorizamos aqui simplemente las categorizo luego hay que hacer un analisis con las variables dummy 
weatherAUSnuevo = weatherAUS

weatherAUSnuevo$WindDir9am = factor(weatherAUS$WindDir9am)

## WindDir3pm

var_WindDir3pm = weatherAUS$WindDir3pm
length(levels(var_WindDir3pm))
ggplot(data=weatherAUS, aes(x=WindDir3pm, y=RainTomorrow)) + geom_bar(stat="identity", position="stack") + theme(axis.text.x=element_text(angle=90,hjust=1))

#comparo con raintomorrow

with(weatherAUS, table(WindDir3pm, RainTomorrow))
ftable(xtabs(~ RainTomorrow + WindDir3pm, data = weatherAUS))



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


ggplot(weatherAUS, aes(x = WindSpeed9am, fill = RainTomorrow)) + 
  geom_dotplot( stackgroups = TRUE, binpositions="all")

## WindSpeed3pm

var_WindSpeed3pm = weatherAUS$WindSpeed3pm
#Medidas de centralidad
summary(var_WindSpeed3pm)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_WindSpeed3pm)

length(var_WindSpeed3pm)
# Basic histogram
ggplot(weatherAUS, aes(x=WindSpeed3pm)) + geom_histogram() + ggtitle('Histograma de WindSpeed3pm')
# Basic Density plot
ggplot(weatherAUS, aes(x = WindSpeed3pm)) + geom_density() + ggtitle('Función de densidad de WindSpeed3pm')
# Basic box plot
ggplot(weatherAUS, aes(y=WindSpeed3pm)) +  geom_boxplot() + ggtitle('Boxplot de WindSpeed3pm')
# Basic scatter plot
ggplot(weatherAUS, aes(x=WindSpeed3pm, y=RainTomorrow)) + geom_point() 

## Humidity9am

var_Humidity9am = weatherAUS$Humidity9am
#Medidas de centralidad
summary(var_Humidity9am)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Humidity9am)

length(var_Humidity9am)
# Basic histogram
ggplot(weatherAUS, aes(x=Humidity9am)) + geom_histogram() + ggtitle('Histograma de Humidity9am')
# Basic Density plot
ggplot(weatherAUS, aes(x = Humidity9am)) + geom_density() + ggtitle('Función de densidad de Humidity9am')
# Basic box plot
ggplot(weatherAUS, aes(y=Humidity9am)) +  geom_boxplot() + ggtitle('Boxplot de Humidity9am')
# Basic scatter plot
ggplot(weatherAUS, aes(x=Humidity9am, y=RainTomorrow)) + geom_point() 

## Humidity3pm

var_Humidity3pm = weatherAUS$Humidity3pm
#Medidas de centralidad
summary(var_Humidity3pm)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Humidity3pm)

length(var_Humidity3pm)
# Basic histogram
ggplot(weatherAUS, aes(x=Humidity3pm)) + geom_histogram() + ggtitle('Histograma de Humidity3pm')
# Basic Density plot
ggplot(weatherAUS, aes(x = Humidity3pm)) + geom_density() + ggtitle('Función de densidad de Humidity3pm')
# Basic box plot
ggplot(weatherAUS, aes(y=Humidity3pm)) +  geom_boxplot() + ggtitle('Boxplot de Humidity3pm')
# Basic scatter plot
ggplot(weatherAUS, aes(x=Humidity3pm, y=RainTomorrow)) + geom_point() 

## Pressure9am

var_Pressure9am = weatherAUS$Pressure9am
#Medidas de centralidad
summary(var_Pressure9am)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Pressure9am)

length(var_Pressure9am)
# Basic histogram
ggplot(weatherAUS, aes(x=Pressure9am)) + geom_histogram() + ggtitle('Histograma de Pressure9am')
# Basic Density plot
ggplot(weatherAUS, aes(x = Pressure9am)) + geom_density() + ggtitle('Función de densidad de Pressure9am')
# Basic box plot
ggplot(weatherAUS, aes(y=Pressure9am)) +  geom_boxplot() + ggtitle('Boxplot de Pressure9am')
# Basic scatter plot
ggplot(weatherAUS, aes(x=Pressure9am, y=RainTomorrow)) + geom_point() 

## Pressure3pm

var_Pressure3pm = weatherAUS$Pressure3pm
#Medidas de centralidad
summary(var_Pressure3pm)
#medidas de dispersión
medidas_dispersion<-function(x) {
  return(list('rango'= range(x), 'Varianza'= var(x), 'Desciación_tipica'= sd(x)))}
#Devuelve NAs porque hay NAs en la columna.
medidas_dispersion(var_Pressure3pm)

length(var_Pressure3pm)
# Basic histogram
ggplot(weatherAUS, aes(x=Pressure3pm)) + geom_histogram() + ggtitle('Histograma de Pressure3pm')
# Basic Density plot
ggplot(weatherAUS, aes(x = Pressure3pm)) + geom_density() + ggtitle('Función de densidad de Pressure3pm')
# Basic box plot
ggplot(weatherAUS, aes(y=Pressure3pm)) +  geom_boxplot() + ggtitle('Boxplot de Pressure3pm')
# Basic scatter plot
ggplot(weatherAUS, aes(x=Pressure3pm, y=RainTomorrow)) + geom_point() 

#Windir & winspeed

weatherAUS %>%
  group_by(WindDir9am) %>%
  summarize(avg_WindSpeed9am = mean(WindSpeed9am)) %>%
  ggplot(aes(x = avg_WindSpeed9am, y = reorder(WindDir9am, avg_WindSpeed9am))) + 
  geom_point(size = 5)

weatherAUS %>%
  group_by(WindDir3pm) %>%
  summarize(avg_WindSpeed3pm = mean(WindSpeed3pm)) %>%
  ggplot(aes(x = avg_WindSpeed3pm, y = reorder(WindDir3pm, avg_WindSpeed3pm))) + 
  geom_point(size = 5)

qplot(log10(WindSpeed9am), log10(WindSpeed3pm), data = weatherAUS)

qplot(log10(WindSpeed9am), log10(WindSpeed3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre velocidades de viento a diferentes horas')

qplot(WindSpeed9am, WindSpeed3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre velocidades de viento a diferentes horas y lluvia al dia siguiente')

weatherAUS %>%
  group_by(WindDir9am, RainTomorrow) %>% 
  summarise(avg_WindSpeed9am = mean(WindSpeed9am)) %>%
  ggplot(aes(x=WindDir9am, y=avg_WindSpeed9am, fill=RainTomorrow)) + geom_bar(stat = "identity",
                                                                              position = "dodge") + 
  ggtitle("relacion entre direccion de viento y velocidad con la lluvia del dia siguiente")

weatherAUS %>%
  group_by(WindDir3pm, RainTomorrow) %>% 
  summarise(avg_WindSpeed3pm = mean(WindSpeed3pm)) %>%
  ggplot(aes(x=WindDir3pm, y=avg_WindSpeed3pm, fill=RainTomorrow)) + geom_bar(stat = "identity",
                                                                              position = "dodge") + 
  ggtitle("relacion entre direccion de viento y velocidad con la lluvia del dia siguiente")

#Humidity 

qplot(log10(Humidity9am), log10(Humidity3pm), data = weatherAUS)

qplot(log10(Humidity9am), log10(Humidity3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre humedad a diferentes horas')

qplot(Humidity9am, Humidity3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre humedad a diferentes horas y lluvia al dia siguiente')


#Pressure

qplot(log10(Pressure9am), log10(Pressure3pm), data = weatherAUS)

qplot(log10(Pressure9am), log10(Pressure3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Pressure9am, Humidity3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')


#humedad y presion

qplot(log10(Pressure9am), log10(Humidity9am), data = weatherAUS)

qplot(log10(Pressure9am), log10(Humidity9am), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Pressure9am, Humidity9am, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')


qplot(log10(Pressure3pm), log10(Humidity3pm), data = weatherAUS)

qplot(log10(Pressure3pm), log10(Humidity3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Pressure3pm, Humidity3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')


#presion y Winspeed

qplot(log10(Pressure9am), log10(WindSpeed9am), data = weatherAUS)

qplot(log10(Pressure9am), log10(WindSpeed9am), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Pressure9am, WindSpeed9am, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')


qplot(log10(Pressure3pm), log10(WindSpeed3pm), data = weatherAUS)

qplot(log10(Pressure3pm), log10(WindSpeed3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Pressure3pm, WindSpeed3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')

#humedad y Winspeed

qplot(log10(Humidity9am), log10(WindSpeed9am), data = weatherAUS)

qplot(log10(Humidity9am), log10(WindSpeed9am), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Humidity9am, WindSpeed9am, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')


qplot(log10(Humidity3pm), log10(WindSpeed3pm), data = weatherAUS)

qplot(log10(Humidity3pm), log10(WindSpeed3pm), data = weatherAUS) +
  geom_smooth(method = "lm") +
  ggtitle('Relación entre presiones a diferentes horas')

qplot(Humidity3pm, WindSpeed3pm, data = weatherAUS, colour = factor(RainTomorrow)) +
  geom_smooth() +
  ggtitle('Relación entre presiones a diferentes horas y lluvia al dia siguiente')



weatherAUSCorr <- weatherAUS %>% select(Temp9am, Temp3pm, Pressure9am,Pressure3pm,MinTemp,MaxTemp) %>% na.omit()
  

numeric <- map_lgl(weatherAUSCorr, is.numeric)

correlations <- cor(weatherAUSCorr[,numeric])

diag(correlations) <- 0

high <- apply(abs(correlations) >= 0.8, 2, any)

corrplot(correlations[high, high], method = "number")


#Valores faltantes

## (categórica)   WindDir9am, WindDir3pm, 
## (cuantitativa) WindSpeed9am, WindSpeed3pm,  Humidity9am, Humidity3pm, Pressure9am, Pressure3pm


weatherAUSFaltantes1 <- weatherAUS %>% select(WindDir9am, WindDir3pm, WindSpeed9am, WindSpeed3pm)

aggr_plot <- aggr(weatherAUSFaltantes1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(weatherAUSFaltantes1), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

weatherAUSFaltantes2 <- weatherAUS %>% select(Humidity9am, Humidity3pm, Pressure9am, Pressure3pm)

aggr_plot <- aggr(weatherAUSFaltantes2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(weatherAUSFaltantes2), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

weatherAUS %>% select(WindSpeed9am, WindSpeed3pm) %>% marginplot()

weatherAUS %>% select(Humidity9am, Humidity3pm) %>% marginplot()

weatherAUS %>% select(Pressure9am, Pressure3pm) %>% marginplot()



weatherAUS %>% select(WindSpeed9am, WindSpeed3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")

weatherAUS %>% select(Humidity9am, Humidity3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")

weatherAUS %>% select(Pressure9am, Pressure3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")


par(mfrow=c(1,1))
train_imputed0 <- mice(train_imputed[,c('Cloud3pm','Cloud9am')], seed=2018, print = F, m = 30)
train_imputed1 <- mice::complete(train_imputed0)
xyplot(train_imputed0, Cloud3pm ~Cloud9am)



#modelo:


lm_fit <- lm(variablerespuesta~., data=Datasetweather)
summary(lm_fit)

#cuando tengamos el dagta set habria qu eelgir cuales vamos a usar, pero mejor usar esta funcion

for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_full, scale=metric)}

regfit_fwd <- leaps::regsubsets(Salary~., Hitters, method="forward")
for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_fwd, scale=metric)}


regfit_bwd <- leaps::regsubsets(Salary~., Hitters, method="backward")
for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_bwd, scale=metric)}

#aqui seria aplicar el modelo

x <- model.matrix(Salary~., Hitters_nona)[,-1]
y <- Hitters_nona$Salary

#lasso
grid <- 10^seq(10,-2,length=100)

set.seed(1234)
cv_result <- cv.glmnet(x,y,alpha=1)
plot(cv_result)

best_lam <- cv_result$lambda.min
out <- glmnet(x,y,alpha=1,lambda = best_lam)
lasso_coef <- predict(out,type="coefficients",s=best_lam)[1:20,]
lasso_coef[lasso_coef!=0]


first_six <- max(which(cv_result$nzero == 6))
my_lam <- cv_result$lambda[first_six]
out_six <- glmnet(x,y,alpha=1,lambda = first_six)
lasso_coef_six <- predict(out_six,type="coefficients")[1:20,]
lasso_coef_six[lasso_coef_six!=0]
















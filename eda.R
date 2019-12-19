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

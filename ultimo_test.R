
gg_miss_upset(train)

#Variables Temp9am & Temp3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Temp9am, Temp3pm) %>% marginplot()

#Posible imputación simple mediante un algoritmo de clustering k-NN (con la función VIM:kNN)
#Parece que tiene sentido:
train %>% select(Temp9am, Temp3pm) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train, variable=c("Temp9am","Temp3pm"))
#test_imputed <- kNN(test, variable=c("Temp9am","Temp3pm"))


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
train %>% select(MinTemp, MaxTemp) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train, variable=c("MaxTemp","MinTemp"))
#test_imputed <- kNN(test, variable=c("MaxTemp","MinTemp"))

#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$MaxTemp,na.rm = T),col=2,main="MaxTemp")
lines(density(train_imputed$MaxTemp),col=3)
plot(density(train$MinTemp,na.rm = T),col=2,main="MinTemp")
lines(density(train_imputed$MinTemp),col=3)

#Variables Pressure3pm & Pressure9am: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Pressure3pm, Pressure9am) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Pressure3pm, Pressure9am) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Pressure3pm","Pressure9am"))
#test_imputed <- kNN(test_imputed, variable=c("Pressure3pm","Pressure9am"))


#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Pressure3pm,na.rm = T),col=2,main="Pressure3pm")
lines(density(train_imputed$Pressure3pm),col=3)
plot(density(train$Pressure9am,na.rm = T),col=2,main="Pressure9am")
lines(density(train_imputed$Pressure9am),col=3)

#Variables Humidity9am & Humidity3pm: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Humidity3pm, Humidity9am) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Humidity3pm, Humidity9am) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Humidity3pm","Humidity9am"))
#test_imputed <- kNN(test_imputed, variable=c("Humidity3pm","Humidity9am"))


#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Humidity3pm,na.rm = T),col=2,main="Humidity3pm")
lines(density(train_imputed$Humidity3pm),col=3)
plot(density(train$Humidity9am,na.rm = T),col=2,main="Humidity9am")
lines(density(train_imputed$Humidity9am),col=3)

trainSelect2 <- train %>% select(RainToday, Rainfall)
gg_miss_upset(trainSelect2)

#Variables Humidity9am & Rainfall: método: imputación simple: KNN
par(mfrow=c(1,1))
train %>% select(Humidity9am, Rainfall) %>% marginplot()
#Parece que tiene sentido:
train %>% select(Humidity9am, Rainfall) %>% VIM::kNN() %>% marginplot(., delimiter="_imp")
train_imputed <- kNN(train_imputed, variable=c("Humidity9am","Rainfall"))
# test_imputed <- kNN(test_imputed, variable=c("Humidity9am","Rainfall"))




#Comprobaciones:
par(mfrow=c(1,2))
plot(density(train$Humidity9am,na.rm = T),col=2,main="Humidity9am")
lines(density(train_imputed$Humidity9am),col=3)
plot(density(train$Rainfall,na.rm = T),col=2,main="Rainfall")
lines(density(train_imputed$Rainfall),col=3)

# Checkea numero de NA
(cols_withNa <- apply(train_imputed, 2, function(x) sum(is.na(x))))


train_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp", "Humidity3pm_imp", "Pressure3pm_imp","Pressure9am_imp")] <- list(NULL)

imputacion_knn_test<-function(train_imputed, test){
  train_imputed["dataset"] <- 'train_imputed'
  test["dataset"] <- 'test'
  dataset_knn = rbind(train_imputed, test) 
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Temp9am","Temp3pm"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("MaxTemp","MinTemp"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Humidity3pm","Humidity9am"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Pressure3pm","Pressure9am"))
  dataset_knn_imputed <- kNN(dataset_knn, variable=c("Humidity9am","Rainfall"))
  train_imputed = subset(dataset_knn_imputed, dataset == 'train')
  test_imputed = subset(dataset_knn_imputed, dataset == 'test')
  test_imputed[c("Temp9am_imp","Temp3pm_imp" ,"MaxTemp_imp","MinTemp_imp", "Humidity9am_imp","Rainfall_imp", "Humidity3pm_imp", "Pressure3pm_imp","Pressure9am_imp","dataset")] <- list(NULL)
  return (test_imputed) }


test_imputed = imputacion_knn_test(train_imputed, test)



train_imputed$RainToday[which(is.na(train_imputed$RainToday))] <- ifelse(train_imputed$Rainfall == 0, "No", "Yes")
test_imputed$RainToday[which(is.na(test_imputed$RainToday))] <- ifelse(test_imputed$Rainfall == 0, "No", "Yes")


train_imputed$RainToday <- ifelse(train_imputed$RainToday == "Yes", 1, 0)
train_imputed$RainToday = as.numeric(train_imputed$RainToday)

train_imputed$RainTomorrow <- ifelse(train_imputed$RainTomorrow  == "Yes", 1, 0)
train_imputed$RainTomorrow  = as.numeric(train_imputed$RainTomorrow )

test_imputed$RainToday <- ifelse(test_imputed$RainToday == "Yes", 1, 0)
test_imputed$RainToday = as.numeric(test_imputed$RainToday)

test_imputed$RainTomorrow <- ifelse(test_imputed$RainTomorrow  == "Yes", 1, 0)
test_imputed$RainTomorrow  = as.numeric(test_imputed$RainTomorrow )


par(mfrow=c(1,1))
train_imputed0 <- mice(train_imputed[,c('Cloud3pm','Cloud9am')], seed=2018, print = F, m = 30)
train_imputed1 <- mice::complete(train_imputed0)
xyplot(train_imputed0, Cloud3pm ~Cloud9am)

par(mfrow=c(1,2))
plot(density(train$Cloud3pm,na.rm = T),col=2,main="Cloud3pm")
lines(density(train_imputed1$Cloud3pm),col=3)
plot(density(train$Cloud9am,na.rm = T),col=2,main="Cloud9am")
lines(density(train_imputed1$Cloud9am),col=3)

train_imputed$Cloud3pm <- train_imputed1$Cloud3pm
train_imputed$Cloud9am <- train_imputed1$Cloud9am

summer <- filter(train, Season == "summer")
evaporationMeanSummer <- mean(summer$Evaporation,na.rm = TRUE)
winter <- filter(train, Season == "winter")
evaporationMeanWinter <- mean(winter$Evaporation,na.rm = TRUE)
spring <- filter(train, Season == "spring")
evaporationMeanSpring <- mean(spring$Evaporation,na.rm = TRUE)
fall <- filter(train, Season == "fall")
evaporationMeanFall <- mean(fall$Evaporation,na.rm = TRUE)

evaporationMeanSummer
evaporationMeanSpring
evaporationMeanFall
evaporationMeanWinter

evaporationMeanSummer <- round(evaporationMeanSummer,1)
evaporationMeanSpring <- round(evaporationMeanSpring,1)
evaporationMeanFall <- round(evaporationMeanFall,1)
evaporationMeanWinter <- round(evaporationMeanWinter,1)

evaporationMeanSummer
evaporationMeanSpring
evaporationMeanFall
evaporationMeanWinter


train_imputed$Evaporation[which(is.na(train_imputed$Evaporation))] <- ifelse(train_imputed$Season == "summer", evaporationMeanSummer,
                                                                             ifelse(train_imputed$Season == "winter", evaporationMeanWinter,
                                                                                    ifelse(train_imputed$Season == "spring", evaporationMeanSpring,
                                                                                           ifelse(train_imputed$Season == "fall", evaporationMeanFall,
                                                                                                  0))))
test_imputed$Evaporation[which(is.na(test_imputed$Evaporation))] <- ifelse(test_imputed$Season == "summer", evaporationMeanSummer,
                                                                           ifelse(test_imputed$Season == "winter", evaporationMeanWinter,
                                                                                  ifelse(test_imputed$Season == "spring", evaporationMeanSpring,
                                                                                         ifelse(test_imputed$Season == "fall", evaporationMeanFall,
                                                                                                0))))

SunshineMeanSummer <- mean(summer$Sunshine,na.rm = TRUE)
SunshineMeanWinter <- mean(winter$Sunshine,na.rm = TRUE)
SunshineMeanSpring <- mean(spring$Sunshine,na.rm = TRUE)
SunshineMeanFall <- mean(fall$Sunshine,na.rm = TRUE)

SunshineMeanSummer
SunshineMeanSpring
SunshineMeanFall
SunshineMeanWinter


SunshineMeanSummer <- round(SunshineMeanSummer,1)
SunshineMeanSpring <- round(SunshineMeanSpring,1)
SunshineMeanFall <- round(SunshineMeanFall,1)
SunshineMeanWinter <- round(SunshineMeanWinter,1)

SunshineMeanSummer
SunshineMeanSpring
SunshineMeanFall
SunshineMeanWinter


train_imputed$Sunshine[which(is.na(train_imputed$Sunshine))] <- ifelse(train_imputed$Season == "summer", SunshineMeanSummer,
                                                                       ifelse(train_imputed$Season == "winter", SunshineMeanWinter,
                                                                              ifelse(train_imputed$Season == "spring", SunshineMeanSpring,
                                                                                     ifelse(train_imputed$Season == "fall", SunshineMeanFall,
                                                                                            0))))

test_imputed$Sunshine[which(is.na(test_imputed$Sunshine))] <- ifelse(test_imputed$Season == "summer", SunshineMeanSummer,
                                                                     ifelse(test_imputed$Season == "winter", SunshineMeanWinter,
                                                                            ifelse(test_imputed$Season == "spring", SunshineMeanSpring,
                                                                                   ifelse(test_imputed$Season == "fall", SunshineMeanFall,
                                                                                          0))))

trainSelect <- train %>% select(WindDir9am, WindDir3pm, WindSpeed9am,WindSpeed3pm)
gg_miss_upset(trainSelect)

imputacion <- train %>%
  group_by(WindDir9am) %>%
  summarize(avg_WindSpeed9am = mean(WindSpeed9am))
imputacion

par(mfrow=c(1,1))
aux <- select(train_imputed,c('Pressure9am','WindSpeed9am','WindDir9am','Pressure3pm','WindSpeed3pm','WindDir3pm'))
aux2 <- select(test_imputed,c('Pressure9am','WindSpeed9am','WindDir9am','Pressure3pm','WindSpeed3pm','WindDir3pm'))


init = mice(aux, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("Pressure9am","Pressure3pm")]=""
meth[c("WindSpeed9am")]="norm"
meth[c("WindSpeed3pm")]="norm"
meth[c("WindDir9am")]="polyreg"
meth[c("WindDir3pm")]="polyreg"
train_imputed0 <- mice(aux, method=meth, predictorMatrix=predM, m=5)
train_imputed1 <- complete(train_imputed0)

init2 = mice(aux2, maxit=0) 
meth = init2$method
predM = init2$predictorMatrix

meth[c("Pressure9am","Pressure3pm")]=""
meth[c("WindSpeed9am")]="norm"
meth[c("WindSpeed3pm")]="norm"
meth[c("WindDir9am")]="polyreg"
meth[c("WindDir3pm")]="polyreg"
test_imputed2 <- mice(aux2, method=meth, predictorMatrix=predM, m=5)
test_imputed3 <- complete(test_imputed2)


par(mfrow=c(1,2))
plot(density(train$WindSpeed9am,na.rm = T),col=2,main="WindSpeed9am")
lines(density(train_imputed1$WindSpeed9am),col=3)
plot(density(train$WindSpeed3pm,na.rm = T),col=2,main="WindSpeed3pm")
lines(density(train_imputed1$WindSpeed3pm),col=3)


train_imputed$WindSpeed9am <- train_imputed1$WindSpeed9am
train_imputed$WindSpeed3pm <- train_imputed1$WindSpeed3pm
train_imputed$WindDir9am <- train_imputed1$WindDir9am
train_imputed$WindDir3pm <- train_imputed1$WindDir3pm

test_imputed$WindSpeed9am <- test_imputed3$WindSpeed9am
test_imputed$WindSpeed3pm <- test_imputed3$WindSpeed3pm
test_imputed$WindDir9am <- test_imputed3$WindDir9am
test_imputed$WindDir3pm <- test_imputed3$WindDir3pm




a <- data.frame(WindSpeed9am= train_imputed$WindSpeed9am, WindSpeed3pm= train_imputed$WindSpeed3pm)
train_imputed$WindGustSpeed[which(is.na(train_imputed$WindGustSpeed))] <- rowMeans(a, na.rm=TRUE)

b <- data.frame(WindSpeed9am= test_imputed$WindSpeed9am, WindSpeed3pm= test_imputed$WindSpeed3pm)
test_imputed$WindGustSpeed[which(is.na(train_imputed$WindGustSpeed))] <- rowMeans(b, na.rm=TRUE)



train_imputed <- dummy_cols(train_imputed, select_columns = c("Location", "Season","WindGustDir","WindDir9am","WindDir3pm"))
test_imputed <- dummy_cols(test_imputed, select_columns = c("Location", "Season","WindGustDir","WindDir9am","WindDir3pm"))

p1 <- train_imputed %>% select(Rainfall, Season) %>%
  na.omit() %>%
  ggplot(aes(x=Rainfall, colour=Season)) +
  geom_density()
#(train_imputed$Rainfall)+0.01
aux <- filter(train_imputed,train_imputed$Rainfall>0)
p2 <- aux %>% mutate(inv_Rainfall = 1/ (Rainfall)) %>%
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
p2
aux %>% select(Rainfall) %>%
  na.omit() %>%
  symbox(~ Rainfall, data = .)


#Resto de variables: siguen distribución normal
qqnorm(train_imputed$MinTemp, ylab="MinTemp")
qqline(train_imputed$MinTemp, col="red")

qqnorm(train_imputed$MaxTemp, ylab="MaxTemp")
qqline(train_imputed$MaxTemp, col="red")

qqnorm(train_imputed$Temp3pm, ylab="Temp3pm")
qqline(train_imputed$Temp3pm, col="red")

qqnorm(train_imputed$Temp9am, ylab="Temp9am")
qqline(train_imputed$Temp9am, col="red")

qqnorm(train_imputed$Pressure9am, ylab="Pressure9am")
qqline(train_imputed$Pressure9am, col="red")

qqnorm(train_imputed$Pressure3pm, ylab="Pressure3pm")
qqline(train_imputed$Pressure3pm, col="red")


dim(train_imputed)
dim(test_imputed)


# check NA
train_imputed %>%
  summarise_each(list(~ sum(is.na(.)) / length(.) * 100)) %>%
  t()





# combinar variables 
train_imputed$AvgTemp <- round(rowMeans(train_imputed[c('MinTemp', 'MaxTemp')], na.rm=TRUE),digits=2)
train_imputed$Temp <- round(rowMeans(train_imputed[c('Temp9am', 'Temp3pm')], na.rm=TRUE),digits=2)
train_imputed$Pressure <- round(rowMeans(train_imputed[c('Pressure9am', 'Pressure3pm')], na.rm=TRUE),digits=2)
train_imputed$Humidity <- round(rowMeans(train[c('Humidity9am', 'Humidity3pm')], na.rm=TRUE),digits=2)
train_imputed$WindSpeed <- round(rowMeans(train[c('WindSpeed9am', 'WindSpeed3pm')], na.rm=TRUE),digits=2)
#Test
test_imputed$AvgTemp <- round(rowMeans(test_imputed[c('MinTemp', 'MaxTemp')], na.rm=TRUE),digits=2)
test_imputed$Temp <- round(rowMeans(test_imputed[c('Temp9am', 'Temp3pm')], na.rm=TRUE),digits=2)
test_imputed$Pressure <- round(rowMeans(test_imputed[c('Pressure9am', 'Pressure3pm')], na.rm=TRUE),digits=2)
test_imputed$Humidity <- round(rowMeans(test[c('Humidity9am', 'Humidity3pm')], na.rm=TRUE),digits=2)
test_imputed$WindSpeed <- round(rowMeans(test[c('WindSpeed9am', 'WindSpeed3pm')], na.rm=TRUE),digits=2)

#Estandarización
numeric_vars <- c("MinTemp","MaxTemp", "Rainfall", "Evaporation", "Sunshine", "WindGustSpeed", 
                  "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm","Pressure9am" ,
                  "Pressure3pm" , "Temp9am", "Temp3pm", "RISK_MM", "AvgTemp", "Temp", "Pressure", "Humidity", "WindSpeed")

preprocessParams <- preProcess(train_imputed[numeric_vars], method=c("center", "scale"))
train_imputed[numeric_vars] <- predict(preprocessParams, train_imputed[numeric_vars])

#Aplicamos la estandarización con las medias y varianzas de train
test_imputed[numeric_vars] <- predict(preprocessParams, test_imputed[numeric_vars])

#Procesado de variables cualitativas: creación variables dummies
train_imputed <- dummy_cols(train_imputed, select_columns = c("Location", 'Season', 'WindGustDir'))
test_imputed <- dummy_cols(test_imputed, select_columns = c("Location", 'Season', 'WindGustDir'))

train_imputed$Location <- NULL
train_imputed$Season <- NULL
train_imputed$Date <- NULL
train_imputed$WindGustDir <- NULL
train_imputed$WindSpeed9am <- NULL
train_imputed$WindSpeed3pm <- NULL
train_imputed$Humidity9am <- NULL
train_imputed$Humidity3pm <- NULL
train_imputed$MinTemp <- NULL
train_imputed$MaxTemp <- NULL
train_imputed$Pressure9am <- NULL
train_imputed$Pressure3pm <- NULL
train_imputed$Temp9am <- NULL
train_imputed$Temp3pm <- NULL



test_imputed$Location <- NULL
test_imputed$Season <- NULL
test_imputed$Date <- NULL
test_imputed$WindGustDir <- NULL
test_imputed$WindDir3pm_NA <- NULL
test_imputed$WindDir9am_NA <- NULL
test_imputed$WindSpeed9am <- NULL
test_imputed$WindSpeed3pm <- NULL
test_imputed$Humidity9am <- NULL
test_imputed$Humidity3pm <- NULL
test_imputed$MinTemp <- NULL
test_imputed$MaxTemp <- NULL
test_imputed$Pressure9am <- NULL
test_imputed$Pressure3pm <- NULL
test_imputed$Temp9am <- NULL
test_imputed$Temp3pm <- NULL

#Selección de variables 

train_imputed_sinNA = na.omit(train_imputed)
x_train = train_imputed_sinNA[, !names(train_imputed_sinNA) %in% c("RainTomorrow")] 

y_train = train_imputed_sinNA$RainTomorrow

test_imputed_sinNA = na.omit(test_imputed)
x_test = test_imputed_sinNA[, !names(test_imputed_sinNA) %in% c("RainTomorrow")] 
y_test = test_imputed_sinNA$RainTomorrow

dim(test_imputed_sinNA)
dim(train_imputed_sinNA)

# Modelo usando Lasso

#Primero vemos Lasso
x = model.matrix(RainTomorrow~ ., train_imputed_sinNA)[,-1]

lambda_seq <- 10^seq(2, -2, by = -.1)
cv.out <- cv.glmnet(x, y_train, alpha = 1, lambda = lambda_seq)

plot(cv.out)
# identifying best lamda
best_lam <- cv.out$lambda.min
lasso_best <- glmnet(x, y_train, alpha = 1, lambda = best_lam)

c<-coef(lasso_best,s=best_lam,exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables


new_train = train_imputed_sinNA %>% select("AvgTemp", "Temp", "Pressure", "Humidity", "WindSpeed", "RainTomorrow") 
  

new_test = test_imputed_sinNA %>% select("AvgTemp", "Temp", "Pressure", "Humidity", "WindSpeed", "RainTomorrow")




#Modelo
#Entrenamiento
glm_model_train = glm(RainTomorrow~ ., data=new_train, family= binomial)

#Test
glm_test = predict(glm_model_train, newdata = new_test, type = "response")

#Evaluación modelo

#Summary
summary(glm_model_train)
#Tabla de ganancia
logistic_gains_table <- blr_gains_table(glm_model_train, data = new_train)
#Curva ROC
blr_roc_curve(logistic_gains_table)
#Matriz de confusión

umbral_dec = 0.46
glm_test <- ifelse(glm_test >= umbral_dec, 1, 0)
glm_test <- factor(glm_test, levels = c(0,1))
tabla_conf <- table(glm_test, new_test$RainTomorrow)
caret::confusionMatrix(tabla_conf, positive = '1')


# Modelo 

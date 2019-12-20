

#Estandarización
numeric_vars <- c("MinTemp","MaxTemp", "Rainfall", "Evaporation", "Sunshine", "WindGustSpeed", 
                  "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm","Pressure9am" ,
                  "Pressure3pm" , "Temp9am", "Temp3pm", "RISK_MM")

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

test_imputed$Location <- NULL
test_imputed$Season <- NULL
test_imputed$Date <- NULL
test_imputed$WindGustDir <- NULL


#Selección de variables 

train_imputed_sinNA = na.omit(train_imputed)
x_train = train_imputed_sinNA[, !names(train_imputed_sinNA) %in% c("RainTomorrow")] 

y_train = train_imputed_sinNA$RainTomorrow

test_imputed_sinNA = na.omit(test_imputed)
x_test = test_imputed_sinNA[, !names(test_imputed_sinNA) %in% c("RainTomorrow")] 
y_test = test_imputed_sinNA$RainTomorrow

dim(test_imputed_sinNA)
dim(train_imputed_sinNA)


new_train = train_imputed_sinNA %>% select("MaxTemp","RainToday","WindGustSpeed", "Humidity3pm", "Pressure3pm",
                                           "RainTomorrow")
new_test = test_imputed_sinNA %>% select("MaxTemp","RainToday","WindGustSpeed", "Humidity3pm", "Pressure3pm",
                                         "RainTomorrow")


weatherAUSCorr <- new_train %>% select(MaxTemp, RainToday, WindGustSpeed, Humidity3pm, Pressure3pm, RainTomorrow) %>% na.omit()

numeric <- map_lgl(weatherAUSCorr, is.numeric)

correlations <- cor(weatherAUSCorr[,numeric])

diag(correlations) <- 0

high <- apply(abs(correlations) >= 0.2, 2, any)

corrplot(correlations[high, high], method = "color")

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




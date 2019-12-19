train_imputed$LogRainfall<- (log(train_imputed$Rainfall + 1)) 
test_imputed$LogRainfall<- (log(test_imputed$Rainfall + 1))

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

test_imputed$Location <- NULL
test_imputed$Season <- NULL
test_imputed$Date <- NULL

#Datasets preparados:
x_train = train_imputed[, !names(train_imputed) %in% c("RainTomorrow")] 
y_train = train_imputed$RainTomorrow

#Selección de variables 

train_imputed_sinNA = na.omit(train_imputed)
x_train = train_imputed_sinNA[, !names(train_imputed_sinNA) %in% c("RainTomorrow")] 
train_imputed_sinNA$RainTomorrow <- ifelse(train_imputed_sinNA$RainTomorrow == "Yes", 1, 0)
train_imputed_sinNA$RainToday <- ifelse(train_imputed_sinNA$RainToday== "Yes", 1, 0)
train_imputed_sinNA$RainTomorrow = as.numeric(train_imputed_sinNA$RainTomorrow)
y_train = train_imputed_sinNA$RainTomorrow

test_imputed_sinNA = na.omit(test_imputed)
x_test = test_imputed_sinNA[, !names(test_imputed_sinNA) %in% c("RainTomorrow")] 
test_imputed_sinNA$RainTomorrow <- ifelse(test_imputed_sinNA$RainTomorrow == "Yes", 1, 0)
test_imputed_sinNA$RainToday <- ifelse(test_imputed_sinNA$RainToday== "Yes", 1, 0)
test_imputed_sinNA$RainTomorrow = as.numeric(test_imputed_sinNA$RainTomorrow)
y_test = test_imputed_sinNA$RainTomorrow

dim(test_imputed_sinNA)
dim(train_imputed_sinNA)

#Primero vemos Lasso
library(glmnet)
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
#Modelo
#Entrenamiento
glm_model_train = glm(RainTomorrow~ ., data=train_imputed_sinNA, family= binomial)
#Test
glm_test = predict(glm_model_train, newdata = test_imputed_sinNA, type = "response")

#Evaluación modelo
library(blorr)
library(magrittr)
#Summary
summary(glm_model_train)
#Tabla de ganancia
logistic_gains_table <- blr_gains_table(glm_model_train, data = train_imputed_sinNA)
#Curva ROC
blr_roc_curve(logistic_gains_table)
#Matriz de confusión
tabla_conf <- table(glm_test, test_imputed_sinNA$RainTomorrow)
caret::confusionMatrix(tabla_conf, positive = '1')


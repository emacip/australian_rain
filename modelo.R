

#Estandarización
numeric_vars <- c("MinTemp","MaxTemp", "Rainfall", "Evaporation", "Sunshine", "WindGustSpeed", 
                  "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm","Pressure9am" ,
                  "Pressure3pm" , "Temp9am", "Temp3pm", "RISK_MM")

preprocessParams <- preProcess(train_imputed[numeric_vars], method=c("center", "scale"))
train_imputed[numeric_vars] <- predict(preprocessParams, train_imputed[numeric_vars])

#Aplicamos la estandarización con las medias y varianzas de train
test_imputed[numeric_vars] <- predict(preprocessParams, test_imputed[numeric_vars])

#Procesado de variables cualitativas: creación variables dummies
train_imputed <- dummy_cols(train_imputed, select_columns = c("Location", 'Season', 'WindGustDir',))
test_imputed <- dummy_cols(train_imputed, select_columns = c("Location", 'Season'))




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
train_imputed_sinNA$RainTomorrow = as.numeric(train_imputed_sinNA$RainTomorrow)
y_train = train_imputed_sinNA$RainTomorrow
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

#Modelo


glm_model_train = glm(RainTomorrow~ .,
                      data=train_imputed_sinNA, family= binomial)
summary(glm_model_train)



blr_regress(glm_model_train)

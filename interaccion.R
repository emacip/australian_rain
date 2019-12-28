# Interaccion 2 variables

summary(train)

boxplot_vars <- function(var, title) {
  
  ggplot(train, aes(x = RainTomorrow, y = var)) +
    geom_violin() +
    theme_hc() + 
    stat_summary(fun.data = "mean_sdl", colour="blue") +
    ylab(title)
  
}


mintemp <- boxplot_vars(train$MinTemp, "MinTemp")
maxtemp <- boxplot_vars(train$MaxTemp, "MaxTemp")
temp9 <- boxplot_vars(train$Temp9am, "Temp9am")
temp3 <- boxplot_vars(train$Temp3pm, "Temp3pm")
press9 <- boxplot_vars(train$Pressure9am, "Pressure9am")
press3 <- boxplot_vars(train$Pressure3pm, "Pressure3pm")

plot_grid(mintemp, maxtemp, temp9, temp3, press9, press3, nrow = 3, ncol = 2)

# combinar variables 
train$AvgTemp <- round(rowMeans(train[c('MinTemp', 'MaxTemp')], na.rm=TRUE),digits=2)
train$Temp <- round(rowMeans(train[c('Temp9am', 'Temp3pm')], na.rm=TRUE),digits=2)
train$Pressure <- round(rowMeans(train[c('Pressure9am', 'Pressure3pm')], na.rm=TRUE),digits=2)
train$Pressure <- round(rowMeans(train[c('Pressure9am', 'Pressure3pm')], na.rm=TRUE),digits=2)

#Test
test$AvgTemp <- round(rowMeans(test[c('MinTemp', 'MaxTemp')], na.rm=TRUE),digits=2)
test$Temp <- round(rowMeans(test[c('Temp9am', 'Temp3pm')], na.rm=TRUE),digits=2)
test$Pressure <- round(rowMeans(test[c('Pressure9am', 'Pressure3pm')], na.rm=TRUE),digits=2)


AvgTemp <- boxplot_vars(train$AvgTemp, "AvgTemp")
temp <- boxplot_vars(train$Temp, "Temp")
press <- boxplot_vars(train$Pressure, "press")


plot_grid(AvgTemp, temp, press, nrow = 3, ncol = 1)



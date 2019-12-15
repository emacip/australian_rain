library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(RColorBrewer)


#medidas de dispersión
medidas_dispersion <- function(x) {
  return(list('Rango'= range(x), 'Varianza'= var(x), 'Desviación_tipica'= sd(x)))}

# Evaporation
evap <- weatherAUS_2$Evaporation
summary(evap)
medidas_dispersion(evap)

# missing data
aggr(evap, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(evap), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = evap)) + geom_histogram()
ggplot(weatherAUS_2, aes(x = evap)) + geom_density()

# boxplot
ggplot(weatherAUS_2, aes(y=evap)) +  geom_boxplot() 

# scatter plot
ggplot(weatherAUS_2, aes(x=weatherAUS_2$Rainfall, y=evap)) + geom_point()

# margin plot
weatherAUS_2 %>% select(Evaporation, Sunshine) %>% marginplot()

# Sunshine
sunshine <- weatherAUS_2$Sunshine
summary(sunshine)
medidas_dispersion(sunshine)

# missing data
aggr(sunshine, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(sunshine), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = sunshine)) + geom_histogram()
ggplot(weatherAUS_2, aes(x = sunshine)) + geom_density()

# boxplot
ggplot(weatherAUS_2, aes(y=sunshine)) +  geom_boxplot() 

# scatter plot
ggplot(weatherAUS_2, aes(x=weatherAUS_2$Rainfall, y=sunshine)) + geom_point()


# WindGustDir
wind_gust_dir <- weatherAUS_2$WindGustDir
summary(wind_gust_dir)


# missing data
aggr(wind_gust_dir, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(wind_gust_dir), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = wind_gust_dir)) + geom_bar()


# WindGustSpeed
wind_gust_speed <- weatherAUS_2$WindGustSpeed
summary(wind_gust_speed)


# missing data
aggr(wind_gust_speed, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(wind_gust_speed), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = wind_gust_speed)) + geom_bar()
ggplot(weatherAUS_2, aes(x = wind_gust_speed)) + geom_density()

# boxplot
ggplot(weatherAUS_2, aes(y=wind_gust_speed)) +  geom_boxplot() 

# scatter plot
ggplot(weatherAUS_2, aes(x=weatherAUS_2$WindGustDir, y=wind_gust_speed)) + geom_point()



# RainToday
rain_today <- weatherAUS_2$RainToday
summary(rain_today)


# missing data
aggr(rain_today, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(rain_today), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = rain_today)) + geom_bar()



# RISK_MM 
# The amount of next day rain in mm. Used to create response variable RainTomorrow. 
# How much rain recorded in millimetres. 

risk_mm <- weatherAUS_2$RISK_MM
summary(risk_mm)


# missing data
aggr(risk_mm, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(risk_mm), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = risk_mm)) + geom_bar()
ggplot(weatherAUS_2, aes(x = risk_mm)) + geom_density()

# boxplot
ggplot(weatherAUS_2, aes(y=risk_mm)) +  geom_boxplot() 

# scatter plot
ggplot(weatherAUS_2, aes(x=weatherAUS_2$RainTomorrow, y=risk_mm)) + geom_point()




# RainTomorrow
rain_tomorrow <- weatherAUS_2$RainTomorrow
summary(rain_tomorrow)


# missing data
aggr(rain_tomorrow, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(rain_tomorrow), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# histogram
ggplot(weatherAUS_2, aes(x = rain_tomorrow)) + geom_bar()





# EDA
ggpairs(weatherAUS_2, columns = 6:8, mapping = ggplot2::aes(colour =weatherAUS_2$Season ))



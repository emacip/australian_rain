library(dplyr)
library(ggplot2)
library(VIM)
library(readr)
library(GGally)



weatherAUS <- read_csv("weatherAUS.csv")
weatherAUS <- filter(weatherAUS, Location == "Sydney")
weatherAUS_WindDir9am <- select(weatherAUS,WindDir9am)
weatherAUS_WindDir9am

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

# EDA 
#summer
summer <- filter(train, Season == "summer")
summer
summer %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainToday) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=summer$RainTomorrow))
#winter
winter <- filter(train, Season == "winter")
winter
winter %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=winter$RainTomorrow))
#spring
spring <- filter(train, Season == "spring")
spring
spring %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=spring$RainTomorrow))
#fall
fall <- filter(train, Season == "fall")
fall
fall %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=fall$RainTomorrow))



new_train = train %>% select("MaxTemp","RainToday","WindGustSpeed", "Humidity3pm", "Pressure3pm",
                                           "RainTomorrow")

new_train %>% 
  ggpairs(columns = 1:6, mapping = ggplot2::aes(colour=fall$RainTomorrow))


vis_dat(train)
vis_miss(data, warn_large_data = FALSE)
vis_miss(train, warn_large_data = FALSE)
(cols_withNa <- apply(data, 2, function(x) sum(is.na(x))))
gg_miss_upset(data)
gg_miss_upset(train)
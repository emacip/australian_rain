# EDA 
#summer
summer <- filter(weatherAUS_2, Season == "summer")
summer
summer %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainToday) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=summer$Location))
#winter
winter <- filter(weatherAUS_2, Season == "winter")
winter
winter %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=winter$RainTomorrow))
#spring
spring <- filter(weatherAUS_2, Season == "spring")
spring
spring %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=spring$RainTomorrow))
#fall
fall <- filter(weatherAUS_2, Season == "fall")
fall
fall %>% select(Evaporation, Sunshine, MinTemp, MaxTemp, RainTomorrow) %>% 
  ggpairs(columns = 1:5, mapping = ggplot2::aes(colour=fall$RainTomorrow))
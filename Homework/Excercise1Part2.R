library(ggplot2)
library(tidyverse)

bike_total = bikeshare %>%
  group_by(hr) %>%
  summarise(bike_avg = mean(total))

ggplot(bike_total)+
  geom_line(aes(x=hr,y= bike_avg))

bike_total1 = bikeshare %>%
  group_by(hr, workingday) %>%
  summarise(bike_avg = mean(total))

ggplot(bike_total1)+
  geom_line(aes(x=hr,y= bike_avg))+
  facet_wrap(~ workingday)

bike_total2 = bikeshare %>%
  filter(hr==8) %>%
  group_by(weathersit,workingday) %>%
  summarise(bike_avg = mean(total))

ggplot(bike_total2)+
  geom_bar(aes(x=weathersit))+
  facet_wrap(~ workingday) 

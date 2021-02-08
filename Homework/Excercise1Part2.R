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


data = bikeshare

sub_data3 = data %>% filter(hr==8) %>% filter(workingday==1) %>% 
  group_by(weathersit) %>% summarize(average_rent = mean(total))
sub_test = data %>% filter(hr==8) %>% filter(workingday==1) %>% 
  summarize(average_rent = mean(total))

sub_data3 = sub_data3 %>% mutate(workingday = 1)
sub_data4 = data %>% filter(hr==8) %>% filter(workingday==0) %>% 
  group_by(weathersit) %>% summarize(average_rent = mean(total))
sub_data4 = sub_data4 %>% mutate(workingday = 0)
merged_data = rbind(sub_data3, sub_data4)

ggplot(merged_data, aes(x = weathersit, y = average_rent, fill = factor
                        (weathersit))) + geom_bar(stat = "identity") + 
  facet_wrap(~workingday) + xlab("Weather Situation") + ylab("Average Rental")
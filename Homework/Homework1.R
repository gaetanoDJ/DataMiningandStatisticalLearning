library(tidyverse)
library(ggplot2)

##### A)
d1 = GasPrices %>%
  group_by(Competitors) %>%
  summarize(Price)
d1

ggplot(data = d1) +
  geom_boxplot(mapping = aes(x=Competitors, y=Price))

# B)
ggplot(data = GasPrices)+
  geom_point(mapping = aes(x = Income, y = Price, color = Brand)) +
  scale_x_continuous(breaks=seq(0,130000, 10000))

# C)
Price_Brand = GasPrices %>%
  group_by(Brand) %>%
  summarize(mean_price=mean(Price))
Price_Brand

test <- ggplot(data = Price_Brand, aes(x=Brand, y=mean_price, fill=Brand))+
  geom_bar(stat = "identity")
test

######### D) 

test1 <- ggplot(data = GasPrices)+
  geom_histogram(aes(x=Price), bins = 30) +
  facet_wrap(~ Stoplight)
test1

######### E) 
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
  geom_point(mapping = aes(x = Income, y = Price, color = Brand),size =3) +
  scale_x_continuous(breaks=seq(0,130000, 10000))

# C)

Price_Brand = GasPrices %>%
  group_by(Brand) %>%
  summarize(mean_price=mean(Price)) 
  

Price_BrandG <- ggplot(data = Price_Brand, aes(x=Brand, y=mean_price, fill=Brand))+
  geom_bar(stat = "identity") +
  ylab('Mean Price') +
  geom_text(aes(label=round(mean_price,digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)
Price_BrandG

######### D) 


test1 <- ggplot(data = GasPrices)+
  geom_histogram(aes(x=Price), bins = 30) +
  facet_wrap(~ Stoplight)
test1



######### E) 
Price_Brand = GasPrices %>%
  group_by(Brand, Highway) %>%
  summarize(mean_price=mean(Price)) 


Price_BrandH <- ggplot(data = Price_Brand, aes(x=Brand, y=mean_price, fill=Brand))+
  geom_bar(stat = "identity") +
  ylab('Mean Price') +
  geom_text(aes(label=round(mean_price,digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)+
  facet_wrap(~ Highway)
Price_BrandH
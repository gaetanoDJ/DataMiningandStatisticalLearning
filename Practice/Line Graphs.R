library(ggplot2)
library(tidyverse)

# filter by partner name
uk_toys = toyimports %>%
  filter(partner_name == 'United Kingdom')

head(uk_toys, 10)

# sum up imports across all different categories
uk_toys_total = uk_toys %>%
  group_by(year) %>%
  summarise(toys = sum(US_report_import))

# PLot the result over time in a line graph 
ggplot(uk_toys_total) + 
  geom_line(aes(x=year,y=toys))

# The x axis is bit weird. Let's manually tell ggplot 
# where to put the axis ticks
ggplot(uk_toys_total) + 
  geom_line(aes(x=year,y=toys)) +
  scale_x_continuous(breaks=1996:2005)

# Let's look at three countries
country_list = c('China','United Kingdom', 'Korea, Rep.')

combined_toys = toyimports %>%
  filter(partner_name %in% country_list) %>% # %in% names that are in the list
  group_by(year, partner_name) %>%
  summarise(toys = sum(US_report_import))
combined_toys

# Plot all three as line graphs
ggplot(combined_toys) +
  geom_line(aes(x=year,y=toys,color=partner_name)) +
  scale_x_continuous(breaks=1996:2005)

# this plot fails because the three time series are on a 
# very different scale.
# Solution: a log scale for the y axis
ggplot(combined_toys) +
  geom_line(aes(x=year,y=toys,color=partner_name)) +
  scale_x_continuous(breaks=1996:2005)+
  scale_y_log10()

# using a y axis with ticks at specific values
ggplot(combined_toys) +
  geom_line(aes(x=year,y=toys,color=partner_name)) +
  scale_x_continuous(breaks=1996:2005)+
  scale_y_log10(breaks = c(5000,10000,50000,1e5,1e6,5e6))


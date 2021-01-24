library(tidyverse)
library(ggplot2)

###
# Part 1: data work flow
# group/pipe/summarize/filter/mutate
###

# Use import dataset button, or change the path name
# to the appropriate location on your computer and use this line
titanic = read.csv('../data/titanic.csv')

# quick peak at the first 
head(titanic)

# Our basic contingency table of proportions
# conditioning on the column variable (passenger)
# This is great for exploration ...
xtabs(~ survived + passengerClass, data = titanic) %>% # the %>% is a pipe command
  prop.table(margin = 2)

# but xtabs is not the best for making plots.
# To generate summaries for plotting, we use:
# pipe + group + summarize
# It's a bit more involved, but fancy plots and summaries are easier
# basic reason: to make fancy plots, we need summaries in "long form."

# Example of pipe/group/summarize
# Group by sex and summarize by counting 
# the total number of passengers and survivors.
# Two notes:
#   1) n() is a tidyverse function that counts cases
#   2) We use the double equal sign (==) to test for equality
titanic %>%
  group_by(sex) %>%
  summarize(total_count = n(),
            mean_age = mean(age),
            surv_count = sum(survived == 'yes'),
            surv_pct = surv_count/total_count)

# We could also just skip the "middle man" variables and
# go straight to percentages:
titanic %>%
  group_by(sex) %>%
  summarize(mean_age = mean(age),
            surv_pct = sum(survived == 'yes')/n())

# using filter let's us focus on a subset of the rows.
# here we look at only females
# note we need quotes around the string 'female'
# they can be single or double quotes
titanic %>%
  filter(sex=='female') %>%
  group_by(passengerClass) %>%
  summarize(total_count = n(),
            surv_pct=sum(survived == 'yes')/n())

# only children
titanic %>%
  filter(age < 18) %>%
  group_by(passengerClass) %>%
  summarize(total_count = n(),
            surv_pct=sum(survived == 'yes')/n())

# Using mutate to add a column
# Let's add an indicator variable for adult or child
# and overwrite our original data frame with the new, augmented data frame.
titanic = titanic %>% 
  mutate(adult = ifelse(age > 18, '18+', 'under18'))
head(titanic)

# Now we can group by multitude variables
titanic %>%
  group_by(sex, adult, passengerClass) %>%
  summarize(surv_pct = sum(survived == 'yes')/n())

# the result is a long-form table of summary stats,
# one row for each group


####
# Part 2: bar plots
####

d1 = titanic %>%
  group_by(sex) %>%
  summarize(surv_pct = sum(survived == 'yes')/n())
d1

# Now we can use d1 to make a barplot of survival percentage by sex.
# Use geom_col to make a barplot
ggplot(data = d1) +
  geom_col(mapping = aes(x=sex, y=surv_pct))

# This workflow is easy to generalize to make gancier plots
# Here we:
#   1) group by sex and if passenger was an adult
#   2) summarize by calculating survival percentage
d2 = titanic %>%
  group_by(sex,adult) %>%
  summarize(surv_pct = sum(survived == 'yes')/n())
d2

# Now the payoof we use our table of summary stats to make a bar plot.
# position = 'dodge' puts bars side by side, rather than stacked
ggplot(data = d2) +
  geom_col(mapping = aes(x=adult,y=surv_pct,fill=sex),
           position = 'dodge')
# We can make two different comparisons from this plot:
#   - survival vs sex, holding age constant
#   - survival vs age, holding sex constant

# Now 3 variables! Add a faceting layer:
d3 = titanic %>%
  group_by(sex,passengerClass, adult) %>%
  summarize(surv_pct = sum(survived == 'yes')/n())
d3

ggplot(data = d3) +
  geom_col(mapping = aes(x=adult,y=surv_pct,fill=sex),
           position = 'dodge') + 
  facet_wrap(~ passengerClass)
# we Can make many different comparisons from this plot:
#   - survival vs sex, holding age and class constant
#   - survival vs class, holding age and sex constant
#   - survival vs age, holding class and sex constant

# With some cleaner lables
ggplot(data = d3) +
  geom_col(mapping = aes(x=adult,y=surv_pct,fill=sex),
           position = 'dodge') + 
  facet_wrap(~ passengerClass) +
  labs(title="Survival on the Titanic",
       y='Fraction surviving',
       x=" Adult?",
       fill="Sex")

# and entirely different organization of the same info.
# this one makes comparisons across class for fixed sex and age more immediate
ggplot(data = d3) +
  geom_col(mapping = aes(x=sex,y=surv_pct,fill= passengerClass),
           position = 'dodge') + 
  facet_wrap(~ adult)

###
# Let's try one more bar graph of summary stats
# This time: means rather than proportions
###

# mean age by class and sex 
d4 = titanic %>%
  group_by(sex,passengerClass) %>%
  summarise(mean_age = mean(age))
d4

# Now a barplot of mean by age class and sex
ggplot(data = d4)+
  geom_col(mapping = aes(x=passengerClass, y=mean_age, fill =sex),
           postion = 'dodge')

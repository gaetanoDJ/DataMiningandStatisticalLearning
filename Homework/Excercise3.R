library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rsample) 
library(gamlr)
Green <- greenbuildings
Green$Revenue <- Green$Rent*Green$leasing_rate
Green$Rent <- NULL
Green$leasing_rate <- NULL
Green$CS_PropertyID <- NULL
Green$cluster <- NULL
Green$LEED <- NULL
Green$Energystar <- NULL


Green_split = initial_split(Green, prop = 0.8)
Green_train = training(Green_split)

Green_test = testing(Green_split)

lm_all = lm(Revenue ~ ., data=Green_train)
lm_step = step(lm_all, 
               scope=~(.))
lm1 <- lm_step
lm_step2 = step(lm1, scope = ~(.)^2)


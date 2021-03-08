library(tidyverse)
library(ggplot2)
library(modelr)
library(rsample)
library(mosaic)
library(foreach)
library(FNN)
library(caret)
### Prblem 1

####A)
capmetro_UT = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))

Metro <- capmetro_UT

MetroBoard = Metro %>%
  group_by(hour_of_day,day_of_week,month) %>%
  summarize(avgboard = mean(boarding))


ggplot(data = MetroBoard)+
  geom_line(mapping = aes(x=hour_of_day, y=avgboard, group = month, colour = month)) +
  facet_wrap(~ day_of_week)


#### B)

MetroTemp = Metro %>%
  group_by(day_of_week)

MetroTemp$Weekend1 = ifelse(Metro$weekend %in% "weekend" , 1, 0)
ggplot(data = MetroTemp)+
  geom_point(mapping = aes(x=temperature, y=boarding, group = factor(Weekend1), colour =factor(Weekend1))) +
  facet_wrap(~ hour_of_day)


####  Problem 2

data(SaratogaHouses)
####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
saratoga_train = training(saratoga_split)
saratoga_test = testing(saratoga_split)

# Fit to the training data
# Sometimes it's easier to name the variables we want to leave out
# The command below yields exactly the same model.
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
lm2 = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
lm3 = lm(price ~ (. - pctCollege - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)
lm4 = lm(price ~ . - lotSize - sewer - landValue, data=saratoga_train)
lm5 = lm(price ~  lotSize + landValue + livingArea + bedrooms + bathrooms + newConstruction +  , data = saratoga_train)

lm6 = lm(price ~ . , data = saratoga_train)
lm7 = lm(price ~ . - sewer - fuel - heating - fireplaces - pctCollege, data = saratoga_train)
coef(lm1) %>% round(0)
coef(lm2) %>% round(0)
coef(lm7) %>% round(0)
coef(lm6) %>% round(0)
summary(lm7)
# Predictions out of sample
# Root mean squared error
rmse(lm1, saratoga_test)
rmse(lm2, saratoga_test)
rmse(lm5, saratoga_test)
rmse(lm6, saratoga_test)
rmse(lm7, saratoga_test)
# Can you hand-build a model that improves on all three?
# Remember feature engineering, and remember not just to rely on a single train/test split


Total = nrow(SaratogaHouses)
saratoga_train = round(Total*0.80)
saratoga_test = (Total-saratoga_train)

RMSE1 <- NULL
RMSE2 <- NULL
RMSE3 <- NULL
RMSE4 <- NULL
RMSE5 <- NULL
RMSE6 <- NULL
RMSE7 <- NULL
RMSE8 <- NULL
for (i in seq(1:500)){
  saratoga_training = sample.int(Total, saratoga_train, replace=FALSE)
  saratoga_testing = setdiff(1:Total,  saratoga_training)

  saratoga_training = SaratogaHouses[saratoga_training,]
  saratoga_testing = SaratogaHouses[saratoga_testing,]
  
lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_training)
lm2 = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction, data=saratoga_training)
lm3 = lm(price ~ (. - pctCollege - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_training) 
lm4 = lm(price ~ . , data = saratoga_training)
lm5 = lm(price ~ . - sewer - fuel - heating - fireplaces - pctCollege, data = saratoga_training)
lm6 = lm(price ~ . + I(livingArea^2) - sewer - fuel - heating - fireplaces - pctCollege, data = saratoga_training)
lm7 = lm(price ~ . + I(livingArea^2)+ I(bedrooms^2)  - sewer - fuel - heating - fireplaces - pctCollege, data = saratoga_training)
lm8 = lm(price ~ . + I(livingArea^2)+ I(bathrooms^2) - sewer - fuel - heating - fireplaces - pctCollege, data = saratoga_training)

#Run it on the actual and the predicted values
RMSE1[i]= rmse(lm1, saratoga_testing)
RMSE2[i]= rmse(lm2, saratoga_testing)
RMSE3[i]= rmse(lm3, saratoga_testing)
RMSE4[i]= rmse(lm4, saratoga_testing)
RMSE5[i]= rmse(lm5, saratoga_testing)
RMSE6[i]= rmse(lm6, saratoga_testing)
RMSE7[i]= rmse(lm7, saratoga_testing)
RMSE8[i]= rmse(lm8, saratoga_testing)
}

mean(RMSE1)
mean(RMSE2)
mean(RMSE3)
mean(RMSE4)
mean(RMSE5)
mean(RMSE6)
mean(RMSE7)
mean(RMSE8)


  Total = nrow(SaratogaHouses)
  saratoga_training = round(Total*0.80)
  saratoga_testing = (Total-saratoga_train)

  saratoga_training = sample.int(Total, saratoga_train, replace=FALSE)
  saratoga_testing = setdiff(1:Total,  saratoga_training)
  
  saratoga_training = SaratogaHouses[saratoga_training,]
  saratoga_testing = SaratogaHouses[saratoga_testing,]
  
  XTrain = model.matrix(~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
                               + rooms + fuel + centralAir + landValue, data=saratoga_training) 
  XTest = model.matrix(~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
                             + rooms + fuel + centralAir + landValue, data=saratoga_testing)
  
  Ytraining = saratoga_training$price
  Ytesting = saratoga_testing$price
  
  scale_training = apply(Xtrain, 2, sd) 
  Xscaled_train = scale(Xtrain, scale = scale_training)
  Xscaled_test = scale(Xtest, scale = scale_training) 
  

  k_grid = seq(2,100) 
  rmse_grid = foreach(K = k_grid, .combine='c') %do% {
    knn_model = knnreg(price ~ Xtrain,saratoga_training, k=K)
    rmse(knn_model,saratoga_testing)
  } 
  
  rmse_grid = data.frame(K = k_grid, RMSE = rmse_grid)

  ggplot(rmse_grid)+
    geom_point(aes(x=K, y=RMSE))+
    labs(y="RMSE", title="RMSE vs k for KNN regression: 65 AMGs")
  
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


ggplot(data = MetroBoard, aes(x=hour_of_day, y=avgboard, group = month, colour = month))+
  geom_line() + xlab("Hour of Day")  + ylab("Average Boarders") + labs(colour='Month')+
  facet_wrap(~ day_of_week)


#### B)

MetroTemp = Metro %>%
  group_by(day_of_week)

MetroTemp$Weekend1 = ifelse(Metro$weekend %in% "weekend" , 1, 0)
ggplot(data = MetroTemp,aes(x=temperature, y=boarding, group = factor(Weekend1), colour =factor(Weekend1)))+
  geom_point() + xlab("Temperature")  + ylab("Boarders") + labs(colour='Type of Day')+
  facet_wrap(~ hour_of_day) +
  scale_color_manual(labels = c("Weekday", "Weekend"), values = c("blue", "red"))


####  Problem 2

data(SaratogaHouses)
####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
saratoga_train = training(saratoga_split)
saratoga_test = testing(saratoga_split)



k_grid = seq(2,100) 
rmse_grid = foreach(K = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ . - sewer - fuel - heating - fireplaces - pctCollege,saratoga_train, k=K)
  rmse(knn_model,saratoga_test)
} 


rmse_grid = data.frame(K = k_grid, RMSE = rmse_grid)

ggplot(rmse_grid)+
  geom_point(aes(x=K, y=RMSE))+
  labs(y="RMSE", title="RMSE vs k for KNN regression: 65 AMGs")



lm_all = lm(price ~ ., data=SaratogaHouses)
summary(lm_all)
stargazer::stargazer(lm_all)
lm_step = step(lm_all, 
               scope=~(.)^2)
stargazer::stargazer(lm_step)
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
RMSE9 <- NULL
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
lm9 = lm(formula = price ~ lotSize + age + landValue + livingArea + 
           pctCollege + bedrooms + fireplaces + bathrooms + rooms + 
           heating + fuel + sewer + waterfront + newConstruction + centralAir + 
           livingArea:centralAir + landValue:newConstruction + bathrooms:heating + 
           livingArea:fuel + age:sewer + age:pctCollege + landValue:fireplaces + 
           livingArea:fireplaces + fireplaces:waterfront + livingArea:waterfront + 
           age:centralAir + fuel:centralAir + bedrooms:fireplaces + 
           lotSize:landValue + bedrooms:waterfront + landValue:bathrooms + 
           pctCollege:newConstruction + heating:waterfront + rooms:heating + 
           bedrooms:fuel + pctCollege:fireplaces + livingArea:pctCollege + 
           lotSize:rooms + heating:sewer + fireplaces:sewer + lotSize:sewer + 
           bedrooms:sewer + bathrooms:sewer + landValue:fuel + fuel:sewer + 
           age:waterfront, data = saratoga_training)
#Run it on the actual and the predicted values
RMSE1[i]= rmse(lm1, saratoga_testing)
RMSE2[i]= rmse(lm2, saratoga_testing)
RMSE3[i]= rmse(lm3, saratoga_testing)
RMSE4[i]= rmse(lm4, saratoga_testing)
RMSE5[i]= rmse(lm5, saratoga_testing)
RMSE6[i]= rmse(lm6, saratoga_testing)
RMSE7[i]= rmse(lm7, saratoga_testing)
RMSE8[i]= rmse(lm8, saratoga_testing)
RMSE9[i]= rmse(lm9, saratoga_testing)
}

mean(RMSE1)
mean(RMSE2)
mean(RMSE3)
mean(RMSE4)
mean(RMSE5)
mean(RMSE6)
mean(RMSE7)
mean(RMSE8)
mean(RMSE9)

  
standardize 

Total = nrow(SaratogaHouses)
  saratoga_training = round(Total*0.80)
  saratoga_testing = (Total-saratoga_train)

  saratoga_training = sample.int(Total, saratoga_training, replace=FALSE)
  saratoga_testing = setdiff(1:Total,  saratoga_training)
  
  saratoga_training = SaratogaHouses[saratoga_training,]
  saratoga_testing = SaratogaHouses[saratoga_testing,]
  
  XTrain = model.matrix(~ . - sewer - fuel - heating - fireplaces - pctCollege, data=saratoga_training) 
  XTest = model.matrix(~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
                             + rooms + fuel + centralAir + landValue, data=saratoga_testing)
  
  Ytraining = saratoga_training$price
  Ytesting = saratoga_testing$price
  
  scale_training = apply(XTrain, 2, sd) 
  Xscaled_train = scale(Xrain, scale = scale_training)
  Xscaled_test = scale(Xtest, scale = scale_training) 
  

  k_grid = seq(2,100) 
  rmse_grid = foreach(K = k_grid, .combine='c') %do% {
    knn_model = knnreg(price ~ XTrain,saratoga_training, k=K)
    rmse(knn_model,saratoga_testing)
  } 
  
  rmse_grid = data.frame(K = k_grid, RMSE = rmse_grid)

  ggplot(rmse_grid)+
    geom_point(aes(x=K, y=RMSE))+
    labs(y="RMSE", title="RMSE vs k for KNN regression: 65 AMGs")
  
  
  
  data(SaratogaHouses)  
  K_folds = 20
  SaratogaHouses_folds =crossv_kfold(SaratogaHouses, k=K_folds)  
  k_grid = seq(2,100) 
  
  cv_grid = foreach(K = k_grid, .combine='c') %do% {
    models = map(SaratogaHouses_folds$train,~ knnreg(price~ . - sewer - fuel - heating - fireplaces - pctCollege, k=K, data = ., use.all=FALSE)) 
    errs = map2_dbl(models, SaratogaHouses$test, modelr::rmse) 
    c(k=K, err =mean(errs), std_err =sd(errs)/sqrt(K_folds))
    } %>% as.data.frame
  
  
ggplot(cv_grid)+
  geom_point(aes(x=K, y=err))+
  geom_errorbar(aes(x=K, ymin = err-std_err, ymax = err+std_err))+
  labs(y="RMSE", title="RMSE vs k for KNN regression: S350s")
  
data(SaratogaHouses)

StanSaratogaHouses <- SaratogaHouses %>%
  mutate_at(c('lotSize',"age",'landValue', "livingArea"), ~(scale(.) %>% as.vector))

saratoga_split = initial_split(StanSaratogaHouses, prop = 0.8)
saratoga_train = training(saratoga_split)
saratoga_test = testing(saratoga_split)



data(SaratogaHouses)

StanSaratogaHouses <- SaratogaHouses %>%
  mutate_at(c('lotSize',"age",'landValue', "livingArea"), ~(scale(.) %>% as.vector))

K_folds = 20
SaratogaHouses_folds =crossv_kfold(StanSaratogaHouses, k=K_folds)


k_grid = seq(2,100, by=2) 
rmse_grid = foreach(k = k_grid, .combine='rbind') %do% {
  knn_model= map(SaratogaHouses_folds$train, ~ knnreg(price ~ . - sewer - fuel - heating - fireplaces - pctCollege,data = ., k=k, use.all = FALSE))
  errs = map2_dbl(knn_model, SaratogaHouses_folds$test, modelr::rmse)
  c(k=k, err = mean(errs))
} %>% as.data.frame

Value=mean(min(rmse_grid$err))

ggplot(rmse_grid)+
  geom_point(aes(x=k, y=err))+
  labs(y="RMSE", title="RMSE vs k for KNN regression", subtitle = Value)
  
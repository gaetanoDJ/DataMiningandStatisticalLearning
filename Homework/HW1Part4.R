library(tidyverse)
library(ggplot2)
library(rsample)
library(caret)
library(foreach)
library(modelr)
library(FNN)

Class350 <- sclass %>%
  filter(trim==350)

##### K

K=416


y_train = Class350$price
x_train = data.frame(mileage=jitter(Class350$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

##### K

K=416


y_train = Class350$price
x_train = data.frame(mileage=jitter(Class350$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

##### K

K=416


y_train = Class350$price
x_train = data.frame(mileage=jitter(Class350$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)


##### K

K=416


y_train = Class350$price
x_train = data.frame(mileage=jitter(Class350$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)


##### K

K=333


y_train = Class350$price
x_train = data.frame(mileage=jitter(Class350$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

Class350_split =  initial_split(Class350, prop=0.8)
Class350_train = training(Class350_split)
Class350_test  = testing(Class350_split)

# train the model and calculate RMSE on the test set
knn_model = knnreg(price ~ mileage, data=Class350_train, k = K)
modelr::rmse(knn_model, Class350_test)



########################

N = nrow(Class350)
N_train = floor(0.8*N)
train_ind = sort(sample.int(N, N_train, replace=FALSE))

D_all = Class350; D_all$set = 'test'; D_all$set[train_ind] = 'train'
D_train = Class350[train_ind,]
D_test = Class350[-train_ind,]





y_train = D_train$price
y_test = D_test$price
X_train = data.frame(mileage=jitter(D_train$mileage))
X_test = data.frame(mileage=jitter(D_test$mileage))


k_grid = unique(round(exp(seq(log(1500), log(2), length=100))))
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class350_train, k = k)
  modelr::rmse(knn_model, Class350_test)
}

################33
k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
           50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_out = foreach(i=1:20, .combine='rbind') %dopar% {
  Class350_split =  initial_split(Class350, prop=0.8)
  Class350_train = training(Class350_split)
  Class350_test  = testing(Class350_split)
  this_rmse = foreach(k = k_grid, .combine='c') %do% {
    # train the model and calculate RMSE on the test set
    knn_model = knnreg(price ~ mileage, data=Class350_train, k = k)
    modelr::rmse(knn_model, Class350_test)
  }
  data.frame(k=k_grid, rmse=this_rmse)
}
rmse_out = arrange(rmse_out, k)
ggplot(rmse_out) + geom_boxplot(aes(x=factor(k), y=rmse)) + theme_bw(base_size=7)

############
k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
            50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_grid_in = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class350_train, k = k)
  modelr::rmse(knn_model, Class350_test)
}

revlog_trans <- function(base = exp(1)) {
  require(scales)
  ## Define the desired transformation.
  trans <- function(x){
    -log(x, base)
  }
  ## Define the reverse of the desired transformation
  inv <- function(x){
    base^(-x)
  }
  ## Creates the transformation
  scales::trans_new(paste("revlog-", base, sep = ""),
                    trans,
                    inv,  ## The reverse of the transformation
                    log_breaks(base = base), ## default way to define the scale breaks
                    domain = c(1e-100, Inf) 
  )
}

rmse_grid_in = data.frame(K = k_grid, RMSE = rmse_grid_in)
ggplot(data=rmse_grid_in) + 
  geom_path(aes(x=K, y=RMSE)) + 
  labs(y = "RMSE (in-sample)") +
  scale_x_continuous(trans=revlog_trans(base = 10))


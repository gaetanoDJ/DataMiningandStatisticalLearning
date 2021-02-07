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

K=2


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

K=3


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

K=25


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

K=100


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

 ###################
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

################RMSE Out-sample

k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
           50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class350_train, k = k)
  modelr::rmse(knn_model, Class350_test)
}

rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)

p_out = ggplot(data=rmse_grid_out) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  scale_x_continuous(trans=revlog_trans(base = 10)) 

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

p_out 
###





############RMSE In-sample
k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
            50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_grid_in = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class350_train, k = k)
  modelr::rmse(knn_model, Class350_train)
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
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  labs(y = "RMSE (in-sample)") +
  scale_x_continuous(trans=revlog_trans(base = 10)) 

######### Graph both

p_out = ggplot(data=rmse_grid_out) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  scale_x_continuous(trans=revlog_trans(base = 10)) 

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

p_out + geom_path(data=rmse_grid_in, aes(x=K, y=RMSE, color='trainset'),size=0.5) + 
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best, color='darkgreen', size=1)

############# Now for the other trim

Class65AMG <- sclass %>%
  filter(trim=='65 AMG')

##### K

K=2


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

##### K

K=3


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

##### K

K=25


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)


##### K

K=100


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)


##### K

K=200


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

Class65AMG_split =  initial_split(Class65AMG, prop=0.8)
Class65AMG_train = training(Class65AMG_split)
Class65AMG_test  = testing(Class65AMG_split)

# train the model and calculate RMSE on the test set
knn_model = knnreg(price ~ mileage, data=Class65AMG_train, k = K)
modelr::rmse(knn_model, Class65AMG_test)

###################
K=292


y_train = Class65AMG$price
x_train = data.frame(mileage=jitter(Class65AMG$mileage))

knn = knnreg(x_train, y_train, k=K)
knn_pred = function(x) {
  predict(knn, newdata=data.frame(mileage=x))
}
g0 = ggplot(data = Class65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey', alpha=0.5)
g0 + stat_function(fun=knn_pred, color='red', size=1, n=1001)

Class65AMG_split =  initial_split(Class65AMG, prop=0.8)
Class65AMG_train = training(Class65AMG_split)
Class65AMG_test  = testing(Class65AMG_split)

# train the model and calculate RMSE on the test set
knn_model = knnreg(price ~ mileage, data=Class65AMG_train, k = K)
modelr::rmse(knn_model, Class65AMG_test)

########################

N = nrow(Class65AMG)
N_train = floor(0.8*N)
train_ind = sort(sample.int(N, N_train, replace=FALSE))

D_all = Class65AMG; D_all$set = 'test'; D_all$set[train_ind] = 'train'
D_train = Class65AMG[train_ind,]
D_test = Class65AMG[-train_ind,]





y_train = D_train$price
y_test = D_test$price
X_train = data.frame(mileage=jitter(D_train$mileage))
X_test = data.frame(mileage=jitter(D_test$mileage))


k_grid = unique(round(exp(seq(log(1500), log(2), length=100))))
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class65AMG_train, k = k)
  modelr::rmse(knn_model, Class65AMG_test)
}

################RMSE Out-sample

k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
           50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class65AMG_train, k = k)
  modelr::rmse(knn_model, Class65AMG_test)
}

rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)

p_out = ggplot(data=rmse_grid_out) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  scale_x_continuous(trans=revlog_trans(base = 10)) 

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

p_out 
###





############RMSE In-sample
k_grid = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45,
           50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)
rmse_grid_in = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=Class65AMG_train, k = k)
  modelr::rmse(knn_model, Class65AMG_train)
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
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  labs(y = "RMSE (in-sample)") +
  scale_x_continuous(trans=revlog_trans(base = 10)) 

######### Graph both

p_out = ggplot(data=rmse_grid_out) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5) + 
  scale_x_continuous(trans=revlog_trans(base = 10)) 

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

p_out + geom_path(data=rmse_grid_in, aes(x=K, y=RMSE, color='trainset'),size=0.5) + 
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best, color='darkgreen', size=1)


library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(tidyverse)


winecopy <- wine
wine1 <- wine[,-(13)]

wine1_scaled = scale(wine1, center=TRUE, scale=TRUE)
wine1_distance_matrix = dist(wine1_scaled, method='euclidean')

X = wine[,-(13)]
X = scale(X, center=TRUE, scale=TRUE)


summary(X)

mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")
clust1 = kmeans(X, 2, nstart=25)

clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[4,]*sigma + mu


which(clust1$cluster == 1)
which(clust1$cluster == 2)


qplot(, color, data=wine, color=factor(clust1$cluster))
qplot(quality, color, data=wine, color=factor(clust1$cluster))

ggplot(wine) + 
  geom_point(aes(fixed.acidity, quality, color=factor(clust1$cluster))) +
  facet_wrap(~ color)

ggplot(wine) + 
  geom_point(aes(citric.acid, quality, color=factor(clust1$cluster))) +
  facet_wrap(~ color)

ggplot(wine) + 
  geom_point(aes(residual.sugar, quality, color=factor(clust1$cluster))) +
  facet_wrap(~ color)

#### PCA

wine <- winecopy
ggplot(wine) + 
  geom_point(aes(x=alcohol, y=total.sulfur.dioxide, color= color))

wine2 <- wine[,-(12:13)]
PCAwine = prcomp(wine2, scale=TRUE, rank=6)
summary(PCAwine)
plot(PCAwine)


loadings_summary1 = PCAwine$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Chemical')

winetest = merge(wine, PCAwine$x[,1:6], by="row.names")
winetest = winetest[,-(1)]

lm1 = lm(quality ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data=winetest)
summary(lm1)


lm2 = lm(quality ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + color, data=winetest)
summary(lm2)

lm2 = lm(color ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data=winetest)
summary(lm2)

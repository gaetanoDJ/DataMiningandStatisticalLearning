library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
library(igraph)

groceries <- read.transactions("C:/Users/gaeta/Documents/GitHub/DataMiningandStatisticalLearning/Data/groceries.txt", sep = ",")
groceriescopy <- groceries


#### Make sure that we correctly imported the data
inspect(groceries[1:10])

#### What are the most frequent items?
summary(groceries)

#### Visualize Freuqency of Items that appear in 5% of the baskets and the top 20 most commonly bought items 
itemFrequencyPlot(groceries, support = 0.05)

itemFrequencyPlot(groceries, topN = 20)

### Plotting Sparse Matrix
image(sample(groceries, 100))


#### Lets now build the rules

groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
inspect(groceryrules)

#### Sort by top 5 lift
inspect(sort(groceryrules, by = "lift")[1:5])
 
#### Lift implie that someone who buys herbs are 4 times more likely to buy root vegetables. 

#### Sort by top support, meaning how frequently it appears
inspect(sort(groceryrules, by = "support")[1:5])

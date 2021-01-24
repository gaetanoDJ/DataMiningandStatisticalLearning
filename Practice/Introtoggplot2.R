library(tidyverse)
library(ggplot2)

# Goals:
# 1) Understand grammar of gprahiocs using ggplot2
# 2) Make a scatterplot that encodes two to three variables
# 3) Learn about faceting: stratifying a basic plot by a third variable

# Loading a data set
data(mpg)

# To see first few lines of the data set
# Every row is a car, the columns is a feature of that car 
head(mpg)

# Basic R plotting command: plot(dataset$x, dataset$y)
plot(mpg$displ, mpg$hwy)

# Pros: simple syntax 
# Cons: not pretty and hard to do complex things

# Use ggplot2 instead.
# Pros: much easier to make complex and beautiful graphs
# Cons: commands are not simple

# Basic structure for all stats graph
# A graphic is a mapping of data variables to
# aesthetic attributes of geometric objects
# all ggplot2 graphs have these three elements
# - a data set (data)
# - a geometry (geom)
# - an aesthetic mapping (aes)

# Below: creating a ggplot with the grammar of graphics
# First layer tells ggplot where to look for variables (data)
# Second layer makes aesthetic mapping (aes) from:
# - data variable displ to aesthetic property x (horizon)
# - data variable hwy to aes property y (vertical)
# It then displays the data in a scatter plot (geom_point)
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy))

#######
# aes is more complicated with > 2 variables
#######

# here we vary map class to point color
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Lots of options for point properties that can be changed.
# Some aes mappings are more effective than others
# I.E. compare the following with our use of color above...

# size of point
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
# we get a warning

# transparency
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
# we get a warning

# point shape
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# we get a warning 

######
# facets
######

# Here we stratify a scatter plot by some third variable (in this example class)
# this is a more succcesful way to show this info than color 

# facet_wrap is dded as a third layer to the plot
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy,)) +
  facet_wrap(~ class, nrow = 2)

# Now adding our own title. caption and axis labels with labs()
# here labs() is the 4th layer added to the previous plot
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy,)) +
  facet_wrap(~ class, nrow = 2) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    caption = "Data from fueleconomy.gov",
    x="Engine displacement (liters)",
    y="Highway gas mileage (mpg)"
  )

######
# MIsc notes
######

# 1) you can save a ggplot as an R object
# try this
p1 = ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy,))

# Now add facet layer to p1
p1 + facet_wrap(~ class, nrow = 2)
p1 + facet_wrap(~ class, nrow = 1)

# 2) You can manually set an aes property by
# placing it _outside_ the aes() command.
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

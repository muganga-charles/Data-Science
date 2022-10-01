############## R FOR DATA SCIENCE ######################################
######## WHAT YOU WILL LEARN #######
##### DATA IMPORTATION: This typically means that you take data stored in a file, database, or web application programming interface (API), and load it into a data frame in R.
##### TIDYING: Storing data consistent form that matches the semantics of the dataset with the way it is stored i.e. columns are variables, rows are observations
#TRANSFORMATION/WRANGLING:Narrowing in on observations of interest (to answer research questions), creating new variables that are functions of existing variables, and calculating a set of summary statistics (like counts or means).
#### VISUALISATION:Displaying data to see patterns. This might raise new questions about the data. A good visualisation might also hint that you’re asking the wrong question, or you need to collect different data. 
#### MODELING: Models are fundamentally mathematical or computational tools used to answer research questions
#### COMMUNICATION:Convey your visual and modelled results to others.


### Set working directory
setwd("~/Daphne/Personal Learning/Data Science/Coding Exercises/R_Scripts")

### Install packages
install.packages("tidyverse")
#This package has several other packages in it that are great for data science
install.packages(c("nycflights13", "gapminder", "Lahman"))
#These packages provide data on airline flights, world development, and baseball that we’ll use to illustrate key data science concepts.

###Call the libraries of the installed packages
library (tidyverse) 
library (nycflights13) 
library (gapminder) 
library (Lahman)


####### 1. DATA VISUALISATION #######
## We will use the mpg dataframe already found in the package ggplot2
#To use another example of dataset in R e.g. iris
datasets::iris
#A data frame is a rectangular collection of variables (in the columns) and observations (in the rows). mpg contains observations collected by the US Environmental Protection Agency on 38 models of car.
ggplot2::mpg
# To view the dataframe
mpg

### 1.2. CREATE A GGPLOT OF THE DATA FRAME MPG ###
# To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# The first argument of ggplot() is the dataset to use in the graph. So ggplot(data = mpg) creates an empty graph
#The function geom_point() adds a layer of points to the plot, which creates a scatterplot. 
# There are different "geom" functions that each add a different type of layer to a plot, making a mapping argument and defining how variables in your dataset are mapped to visual properties.
# The mapping argument is always paired with aes() which can display a scatterplot or graph
# The x and y arguments of aes() specify which variables to map to the x and y axes

## Turn the scatterplot in any other type of graph using the geom function
?geom_histogram
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

### Questions ###
#1. Run ggplot(data = mpg). What do you see?
#2. How many rows are in mpg? How many columns? 
str(mpg)

#3.What does the drv variable describe? 
?mpg
#4. Make a scatterplot of hwy vs cyl.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))
#5. What happens if you make a scatterplot of class vs drv? Is the plot useful?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

###1.3. GENERATING COLOURFUL PLOTS ###
## To change the aesthetics of the plots in ggplot2.  For example, you can map the colors of your points to the class variable to reveal the class of each car.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))
# The colors reveal that many of the unusual points (in orange) are two-seater cars. These cars don’t seem like hybrids, and are, in fact, sports cars!
# The size aesthetic can also be used but one has to be careful as it might not show much distinction
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class, colour = class))

# Can also map class to the alpha aesthetic, which controls the transparency of the points.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
# Or using the shape aesthetic, which controls the shape of the points.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# ggplot automatically assigns only 6 shapes. So this can be changed manually to include other shapes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)+ 
               scale_shape_manual(values = c(7, 8, 9, 21, 22, 23, 24))
             
### QUESTIONS#####
#1. Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg?
?mpg
str(mpg)
#2. Map a continuous variable to colour, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?
  
#3. What happens if you map the same variable to multiple aesthetics?
  
#4. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
?geom_point
#5. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.


#####1.4. FACETS: Adding additional variables with aesthetics###
## Or splitting plots/graphs by each variable ##
# To facet your plot by a single variable, use facet_wrap() function
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# The nrow=2 is the number of rows of plots to generate
# To facet your plot on the combination of two variables, add facet_grid() function
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
# If you prefer to not facet in the rows or columns dimension, use a "." instead of a variable name e.g.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

## QUESTIONS###
#1. What happens if you facet on a continuous variable? Continuous data are very desirable in inferential statistics. however, they tend to be less useful in data mining and are frequently recorded into discrete data or sets (categories),
#E.g. cty or displ
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty, nrow = 2)
#2. What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))
#3. What plots does the following code make? What does . do?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
# There are no rows or column partitions
#4. Take the first faceted plot. What are the advantages to using faceting instead of the colour aesthetic? What are the disadvantages? How might the balance change if you had a larger dataset?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
#5. When using facet_wrap (), what does nrow do? What does ncol do? What other options control the layout of the individual panels? Why doesn’t facet_grid() have nrow and ncol arguments?

#####1.5. GEOMETRIC OBJECTS: Using other "geom" functions to generate different plots/graphs
# A geom is the geometrical object that a plot uses to represent data
#E.g. A scatter plot is a geom_point() function
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# Adding a smooth line/ trend line to the scatter plot requires a geom_smooth() function.
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
# geom_smooth() will draw a different line, with a different linetype, for each unique value of the variable that you map to linetype.
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
# The cars are separated into three lines based on their drv value, which describes a car’s drivetrain (4-four wheel, f-frontwheel, r-rearwheel)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

sv <- c(rep(1,4),rep(2,16),rep(3,90),rep(4,70),rep(5,20))
plot.ecdf(sv)
# Define a variable states to be the state names from the murders data frame
library(dslabs)
data(murders)
states <- murders$state
# Define a variable ranks to detemine the population size ranks
ranks <- rank(murders$population)
# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(states[ind],ranks[ind]) 
my_df

# Compute the average, for entries of na_example that are not NA 
na_example <- c(1,2,3,NA,5,NA,7,NA,9)
na_example <- na.omit(na_example)
View(na_example)
mean(na_example,na.rm=TRUE)
# Compute the average, for entries of na_example that are not NA using the !
mean(na_example[!is.na(na_example)])

# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp <- (temp - 32) * 5/9

# Create a data frame `city_temps`
city_temps <- data.frame(city, temp)

# Define an object `x` with the numbers 1 through 100
x <- 1:100
#Compute the sum 1 + 1/2^2 + 1/3^2 + ... + 1/100^2
sum(1/x^2)
# Load the data
library(dslabs)
data(murders)

# Store the per 100,000 murder rate for each state in murder_rate
murder_rate <- murders$total/murders$population * 100000

# Calculate the average murder rate in the US 
murders[murders$state== "US",]

# removing outliers.
data <- iris[,1:4]
dim(data)
## [1] 150   4
 
quartiles <- quantile(data$Sepal.Width, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$Sepal.Width)
 
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
 
data_no_outlier <- subset(data, data$Sepal.Width > Lower & data$Sepal.Width < Upper)
 
dim(data_no_outlier)
## [1] 146   4
str(PlantGrowth)

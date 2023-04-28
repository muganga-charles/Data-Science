########ASSIGNMENT 2: ANSWER SCRIPT #######
##Packages
library(readxl)
library(tidyverse)
library(dplyr)
setwd

#Import and view dataset
Assignment2_dataset <- read_excel(("D:/R/Assignment2/Assignment2_dataset.xlsx"))
View(Assignment2_dataset)

#Question 1: 
Assignment2_New <- na.omit(Assignment2_dataset) 
##OR
Assignment2_New <- na.exclude(Assignment2_dataset) 


#Question 2:
#Show relationship between prices and perceptions
#define values for perception
## compute the percentage of each perception that characterizea our responses to price.
?length
Positive <- ((length(c(Assignment2_New$P[Assignment2_New$P == "Positive"])))/(length(c(Assignment2_New$P))))*100
Negative <- ((length(c(Assignment2_New$P[Assignment2_New$P == "Negative"])))/(length(c(Assignment2_New$P))))*100
Somewhat_Positive <- ((length(c(Assignment2_New$P[Assignment2_New$P == "SP"])))/(length(c(Assignment2_New$P))))*100
Somewhat_Negative <- ((length(c(Assignment2_New$P[Assignment2_New$P == "SN"])))/(length(c(Assignment2_New$P))))*100
No_Response <- ((length(c(Assignment2_New$P[Assignment2_New$P == "NR"])))/(length(c(Assignment2_New$P))))*100

#Display percentage of all perceptions
Negative
Somewhat_Negative
Positive
Somewhat_Positive
No_Response

#OR display covariation between perception and price
ggplot(data = Assignment2_New, mapping = aes(x = Price)) +
  geom_freqpoly(mapping = aes(color = PC), binwidth = 500)

####Question3:
#Compare perception change(PC) and diamond quality (cut)
#covariation between PC and cut
Perception_Change <- Assignment2_New$PC
Cut <- Assignment2_New$cut
ggplot(data = Assignment2_New) +
  geom_count(mapping = aes(x = Cut, y = Perception_Change))

  
###Question 4:
#Generate a boxplot of all the variables
boxplot(Assignment2_New$carat, Assignment2_New$depth, Assignment2_New$price, Assignment2_New$x, Assignment2_New$y, names=c("carat", "depth","price", "x", "y"), horizontal=TRUE, col="blue", main="Distribution of variables", xlab="Values", ylab="Variables")

##OR generate individual box plots to better view the outliers
boxplot(Assignment2_New$carat)$out

####Question5:
#Generate individual boxplots for each variable
boxplot(Assignment2_New$price)
boxplot(price~x, data = Assignment2_New)
install.packages("ggstatsplot")
library(ggstatsplot)
#Create a boxplot that labels the outliers
ggbetweenstats(data = Assignment2_New, 
               x = x,
               y = price,
               outlier.tagging = TRUE)

ggbetweenstats(Assignment2_New, x, price, outlier.tagging = TRUE)

#OR use the IQR method
#calculate the IQR
IQRprices <- (Assignment2_New$price)
IQRprices
#calculate the quartiles
#For price.
quartileprice <- quantile(Assignment2_New$price, probs=c(.25, .75), na.rm = FALSE)
quartileprice
#calculate the upper and lower limits
pricelower <- quartileprice[1] - 1.5*IQRprices
priceupper <- quartileprice[2] + 1.5*IQRprices
pricelower
priceupper
#filter out the outliers for price
price_no_outliers <- subset(Assignment2_New, Assignment2_New$price > priceupper | Assignment2_New$price < pricelower)
price_no_outliers
boxplot(price_no_outliers$price)
## save the new dataframe with no outliers
Assignment2_New_price <- Assignment2_New[Assignment2_New$price < priceupper & Assignment2_New$price > pricelower,]

#For price/carat (x).
IQRx <- (Assignment2_New$x)
IQRx
quartile_x <- quantile(Assignment2_New$x, probs=c(.25, .75), na.rm = FALSE)
quartile_x
#calculate the upper and lower limits
xlower <- quartileprice[1] - 1.5*IQRprices
xupper <- quartileprice[2] + 1.5*IQRprices
xlower
xupper
#filter out the outliers for x
x_no_outliers <- subset(Assignment2_New, Assignment2_New$x > xupper | Assignment2_New$x < xlower)
x_no_outliers
boxplot(x_no_outliers$x)
Assignment2_New_x <- Assignment2_New[Assignment2_New$x < priceupper & Assignment2_New$x > pricelower,]

####Question 6:
write.csv(Assignment2_New,'Assignment2_Daphne.csv')
write.csv(Assignment2_New_x, file="E:/Documents/Personal Learning/Data Science/Assignments/Assignment 2/Assignment2_Daphne.csv")
Dataset <- write.csv(Assignment2_New_x, file="E:/Documents/Personal Learning/Data Science/Assignments/Assignment 2/Assignment2_Daphne.csv")
view(Dataset)
###Question 7:
ggplot(Dataset, aes(x=cut, y=depth)) + geom_bar(stat="identity", fill="purple") + labs(title="Relationship between the cut and the depth", x="Cut", y="depth")

###Question 8:
##Compare mean, median and mode
#Calculating the mean and median of the variable "carat" in the diamonds dataset
mean(Dataset$carat,  na.rm = TRUE)
median(Dataset$carat,  na.rm = TRUE)

##OR 
# Visualize data using box plots
ggboxplot(Dataset$carat, 
          ylab = "Daimond carat", xlab = FALSE,
          ggtheme = theme_minimal())
#OR
#Q-Q plots draw the correlation between a given sample and the normal distribution.
ggqqplot(Dataset$carat, ylab = "Diamond carat",
         ggtheme = theme_minimal())
#OR
# One-sample wilcoxon test
res <- wilcox.test(Dataset$carat, mu = 0) #mu is theoretical mean (0)


###QUESTION 9:

# Compute the variance between three groups; diamond carat, perception change and price
#view the groups
groups <- Dataset[,c("carat", "PC", "price")] 
groups
#generate ramdom sample of the data
sample <- sample_n(groups, 100)
sample
#the factors are the groups
#show the levels
#use factor function to define the groups
x <- factor(sample$PC)
x
levels(x)
#show the summary of the data by the groups
x <- ordered(x, levels=c("Negative","Positive","NR","SN","SP"))# the order of the levels
library(dplyr)
group_by(groups, PC) %>% 
  summarise(
    mean=mean(price), 
    sd=sd(price), n=n())

#plot the data
library(ggpubr)
#plot the price by the Pc and the carat by the PC
#ggboxplot(groups, x = "PC", y = "price", color = "PC", palette = "jco",add="jitter",order = c("carat", "PC", "price"), xlab = "Perception Change", ylab = "Price", title = "Price by the groups") 

#plot as a line graph
ggline(groups, x = "PC", y = "price", 
       add = c("mean_se"),
       order = c("Negative","Positive","NR","SN","SP"),
       ylab = "Price", xlab="Perception Change", 
       title = "Line plot of the groups")

#plot as a line grap
ggline(groups, x = "PC", y = "carat", 
       add = c("mean_se"),
       order = c("Negative","Positive","NR","SN","SP"),
       ylab = "carat", xlab="Perception Change", 
       title = "Line plot of the groups")

#Compute variance between groups:
#compute the anova to anwser the question
anova <- aov(price ~ PC, data=groups)
summary(anova)

#Question 10(b)
TukeyHSD(anova, conf.level = .95)
plot(TukeyHSD(anova, conf.level = .95))
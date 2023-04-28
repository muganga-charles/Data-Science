library(readxl)
library(ggplot2)
Coffee_data <- read_excel("D:/R/R/Group Assignment 1_Cup sensory results-CWDr lines.xlsx")
View(Coffee_data)
#####part a#####
Origin.Mukono <- Coffee_data[Coffee_data$ORIGIN == "Mukono",]
View(Origin.Mukono)

Origin.Mityana <- Coffee_data[Coffee_data$ORIGIN == "Mityana",]
View(Origin.Mityana)

Origin.Ibanda <- Coffee_data[Coffee_data$ORIGIN == "Ibanda",]
View(Origin.Ibanda)

#Descriptive statistics for the induvidual variables.
AromaMean <- mean(Coffee_data$"FRAGRANCE/AROMA")
FlavorMean <- mean(Coffee_data$FLAVOR)
SaltAcidMean <- mean(Coffee_data$"SALT/ ACID")
BitterSweetMean <- mean(Coffee_data$"BITTER/ SWEET")
AftertasteMean  <- mean(Coffee_data$AFTERTASTE)
MouthfeelMean  <- mean(Coffee_data$"MOUTH FEEL")

variables <- c("FRAGRANCE/AROMA", "FLAVOR", "SALT/ ACID","BITTER/ SWEET","AFTERTASTE","MOUTH FEEL")
variable.mean <- c(AromaMean,FlavorMean,SaltAcidMean,BitterSweetMean,AftertasteMean,MouthfeelMean)
variable_data <- data.frame(variables,variable.mean)
View(variable_data)
#plotting the mean of the variables
ggplot(variable_data, aes(x = variables, y = variable.mean)) + 
    geom_bar(stat = "identity", fill = "#0044ff") + labs(title = "Mean of the variables", x = "Variables", y = "Mean")


#descriptive statistics median overall
Aromamedian <- median(Coffee_data$"FRAGRANCE/AROMA")
Flavormedian <- median(Coffee_data$FLAVOR)
SaltAcidmedian <- median(Coffee_data$"SALT/ ACID")
BitterSweetmedian <- median(Coffee_data$"BITTER/ SWEET")
Aftertastemedian  <- median(Coffee_data$AFTERTASTE)
Mouthfeelmedian  <- median(Coffee_data$"MOUTH FEEL")

variable.median <- c(Aromamedian,Flavormedian,SaltAcidmedian,BitterSweetmedian,Aftertastemedian,Mouthfeelmedian)

variable_data1 <- data.frame(variables,variable.median)
View(variable_data1)

#plotting the median of the variables
ggplot(variable_data1, aes(x = variables, y = variable.median)) + 
    geom_bar(stat = "identity", fill = "#500530") + labs(title = "Median of the variables", x = "Variables", y = "Median")

##
#descriptive statistics range overall
Aromarange <- range(Coffee_data$"FRAGRANCE/AROMA")
Flavorrange <- range(Coffee_data$FLAVOR)
SaltAcidrange <- range(Coffee_data$"SALT/ ACID")
BitterSweetrange <- range(Coffee_data$"BITTER/ SWEET")
Aftertasterange  <- range(Coffee_data$AFTERTASTE)
Mouthfeelrange  <- range(Coffee_data$"MOUTH FEEL")

variable.range <- c(Aromarange,Flavorrange,SaltAcidrange,BitterSweetrange,Aftertasterange,Mouthfeelrange)

variable_data1 <- data.frame(variables,variable.range)
View(variable_data1)

#plotting the range of the variables
ggplot(variable_data1, aes(x = variables, y = variable.range)) + 
    geom_bar(stat = "identity", fill = "#e7380c") + labs(title = "range of the variables", x = "Variables", y = "Range")


###specifing the mean for only mukono###
mukonoAromaMean <- mean(ORIGIN.Mukono$"FRAGRANCE/AROMA")
mukonoFlavorMean <- mean(ORIGIN.Mukono$FLAVOR)
mukonoSaltAcidMean <- mean(ORIGIN.Mukono$"SALT/ ACID")
mukonoBitterSweetMean <- mean(ORIGIN.Mukono$"BITTER/ SWEET")
mukonoAftertasteMean  <- mean(ORIGIN.Mukono$AFTERTASTE)
MukonoMouthfeelMean  <- mean(ORIGIN.Mukono$"MOUTH FEEL")

variable.Mukonomean <- c(mukonoAromaMean,mukonoFlavorMean,mukonoSaltAcidMean,mukonoBitterSweetMean,mukonoAftertasteMean,MukonoMouthfeelMean)

variable_data_mukono <- data.frame(variables,variable.Mukonomean)
View(variable_data_mukono)
#plotting the mean of the variables
ggplot(variable_data_mukono, aes(x = variables, y = variable.Mukonomean)) + 
    geom_bar(stat = "identity", fill = "red") + labs(title = "Mean of the variables in Mukono", x = "Variables", y = "Mean")

#
#descriptive statistics mukono median overall
mukonoAromamedian <- median(ORIGIN.Mukono$"FRAGRANCE/AROMA")
mukonoFlavormedian <- median(ORIGIN.Mukono$FLAVOR)
mukonoSaltAcidmedian <- median(ORIGIN.Mukono$"SALT/ ACID")
mukonoBitterSweetmedian <- median(ORIGIN.Mukono$"BITTER/ SWEET")
mukonoAftertastemedian  <- median(ORIGIN.Mukono$AFTERTASTE)
mukonoMouthfeelmedian  <- median(ORIGIN.Mukono$"MOUTH FEEL")

mukonovariable.median <- c(mukonoAromamedian,mukonoFlavormedian,mukonoSaltAcidmedian,mukonoBitterSweetmedian,mukonoAftertastemedian,mukonoMouthfeelmedian)

mukonovariable_data1 <- data.frame(variables,mukonovariable.median)
View(mukonovariable_data1)

#plotting the median of the variables
ggplot(mukonovariable_data1, aes(x = variables, y = mukonovariable.median)) + 
  geom_bar(stat = "identity", fill = "#500530") + labs(title = "Median of the variables in Mukono ", x = "Variables", y = "Median")


#descriptive statistics range overall
mukonoAromarange <- range(ORIGIN.Mukono$"FRAGRANCE/AROMA")
mukonoFlavorrange <- range(ORIGIN.Mukono$FLAVOR)
mukonoSaltAcidrange <- range(ORIGIN.Mukono$"SALT/ ACID")
mukonoBitterSweetrange <- range(ORIGIN.Mukono$"BITTER/ SWEET")
mukonoAftertasterange  <- range(ORIGIN.Mukono$AFTERTASTE)
mukonoMouthfeelrange  <- range(ORIGIN.Mukono$"MOUTH FEEL")

mukonovariable.range <- c(mukonoAromarange,mukonoFlavorrange,mukonoSaltAcidrange,mukonoBitterSweetrange,mukonoAftertasterange,mukonoMouthfeelrange)

mukonovariable_data1 <- data.frame(variables,mukonovariable.range)
View(mukonovariable_data1)

#plotting the range of the variables
ggplot(mukonovariable_data1, aes(x = variables, y = mukonovariable.range)) + 
  geom_bar(stat = "identity", fill = "#e7380c") + labs(title = "range of the variables in Mukono", x = "Variables", y = "Range")



#### part b ####
#Showing the relationship between the variables
# Aroma and Flavor using scatter plot
ggplot(Coffee_data, aes(x = Coffee_data$"FRAGRANCE/AROMA", y = Coffee_data$FLAVOR)) +
  geom_point() + labs(title = "Aroma and Flavor", x = "Aroma", y = "Flavor")
# Aroma and Salt/Acid
ggplot(Coffee_data, aes(x = Coffee_data$"FRAGRANCE/AROMA", y = Coffee_data$"SALT/ ACID")) +
  geom_point() + labs(title = "Aroma and Salt/Acid", x = "Aroma", y = "Salt/Acid")
# Aroma and Bitter/Sweet
ggplot(Coffee_data, aes(x = Coffee_data$"FRAGRANCE/AROMA", y = Coffee_data$"BITTER/ SWEET")) +
  geom_point() + labs(title = "Aroma and Bitter/Sweet", x = "Aroma", y = "Bitter/Sweet")
# Aroma and Aftertaste
ggplot(Coffee_data, aes(x = Coffee_data$"FRAGRANCE/AROMA", y = Coffee_data$AFTERTASTE)) +
  geom_point() + labs(title = "Aroma and Aftertaste", x = "Aroma", y = "Aftertaste")
# Aroma and Mouthfeel
ggplot(Coffee_data, aes(x = Coffee_data$"FRAGRANCE/AROMA", y = Coffee_data$"MOUTH FEEL")) +
  geom_point() + labs(title = "Aroma and Mouthfeel", x = "Aroma", y = "Mouthfeel")
  
# Flavor and Salt/Acid
ggplot(Coffee_data, aes(x = Coffee_data$FLAVOR, y = Coffee_data$"SALT/ ACID")) +
    geom_point() + labs(title = "Flavor and Salt/Acid", x = "Flavor", y = "Salt/Acid")
# Flavor and Bitter/Sweet
ggplot(Coffee_data, aes(x = Coffee_data$FLAVOR, y = Coffee_data$"BITTER/ SWEET")) + 
    geom_point() + labs(title = "Flavor and Bitter/Sweet", x = "Flavor", y = "Bitter/Sweet")
# Flavor and Aftertaste
ggplot(Coffee_data, aes(x = Coffee_data$FLAVOR, y = Coffee_data$AFTERTASTE)) + 
    geom_point() + labs(title = "Flavor and Aftertaste", x = "Flavor", y = "Aftertaste")
#ggplot(Coffee_data, aes(FLAVOR,AFTERTASTE)) +
#geom_point()

# Flavor and Mouthfeel
ggplot(Coffee_data, aes(x = Coffee_data$FLAVOR, y = Coffee_data$"MOUTH FEEL")) + 
    geom_point() + labs(title = "Flavor and Mouthfeel", x = "Flavor", y = "Mouthfeel")
# Salt/Acid and Bitter/Sweet
ggplot(Coffee_data, aes(x = Coffee_data$"SALT/ ACID", y = Coffee_data$"BITTER/ SWEET")) + 
    geom_point() + labs(title = "Salt/Acid and Bitter/Sweet", x = "Salt/Acid", y = "Bitter/Sweet")
# Salt/Acid and Aftertaste
ggplot(Coffee_data, aes(x = Coffee_data$"SALT/ ACID", y = Coffee_data$AFTERTASTE)) + 
    geom_point() + labs(title = "Salt/Acid and Aftertaste", x = "Salt/Acid", y = "Aftertaste")
# Salt/Acid and Mouthfeel
ggplot(Coffee_data, aes(x = Coffee_data$"SALT/ ACID", y = Coffee_data$"MOUTH FEEL")) + 
    geom_point() + labs(title = "Salt/Acid and Mouthfeel", x = "Salt/Acid", y = "Mouthfeel")
# Bitter/Sweet and Aftertaste
ggplot(Coffee_data, aes(x = Coffee_data$"BITTER/ SWEET", y = Coffee_data$AFTERTASTE)) + 
    geom_point() + labs(title = "Bitter/Sweet and Aftertaste", x = "Bitter/Sweet", y = "Aftertaste")
# Bitter/Sweet and Mouthfeel
ggplot(Coffee_data, aes(x = Coffee_data$"BITTER/ SWEET", y = Coffee_data$"MOUTH FEEL")) + 
    geom_point() + labs(title = "Bitter/Sweet and Mouthfeel", x = "Bitter/Sweet", y = "Mouthfeel")
# Aftertaste and Mouthfeel
ggplot(Coffee_data, aes(x = Coffee_data$AFTERTASTE, y = Coffee_data$"MOUTH FEEL")) + 
    geom_point() + labs(title = "Aftertaste and Mouthfeel", x = "Aftertaste", y = "Mouthfeel")

########part c (i)#########
mean_mityana <- mean(Origin.Mityana$OVERALL)
mean_ibanda <- mean(Origin.Ibanda$OVERALL)
mean_mukono <- mean(Origin.Mukono$OVERALL)

District_names <- c("Mityana","Ibanda","Mukono")
District_means <- c(mean_mityana,mean_ibanda,mean_mukono)

District_data <- data.frame(District_names,District_means)

ggplot(District_data, aes(District_names,District_means,group=1)) +
  geom_line() +
  geom_point()+ labs(title="Line graph for the means of variables in different districts ")

#########part c(ii)###########
KR3 <- Coffee_data[Coffee_data$VARIETY == "KR3",]
KR4 <- Coffee_data[Coffee_data$VARIETY == "KR4",]
KR5 <- Coffee_data[Coffee_data$VARIETY == "KR5",]
KR6 <- Coffee_data[Coffee_data$VARIETY == "KR6",]
KR7 <- Coffee_data[Coffee_data$VARIETY == "KR7",]

mean_KR3 <- mean(KR3$OVERALL)
mean_KR4 <- mean(KR4$OVERALL)
mean_KR5 <- mean(KR5$OVERALL)
mean_KR6 <- mean(KR6$OVERALL)
mean_KR7 <- mean(KR7$OVERALL)

variety_names <- c("KR3","KR4","KR5","KR6","KR7")
variety_means <- c(mean_KR3,mean_KR4,mean_KR5,mean_KR6,mean_KR7)

variety_dataframe <- data.frame(variety_names,variety_means)

ggplot(variety_dataframe, aes(variety_names,variety_means,group=1)) +
  geom_line() +
  geom_point()+ labs(title="Line graph means of the variables of each of the variables and the varities")

#####part d ####
#The distribution central tendencies of each variable
summary(Coffee_data)
# The distribution central tendencies of the FRAGRANCE/AROMA using box plot
boxplot(Coffee_data$"FRAGRANCE/AROMA", main = "FRAGRANCE/AROMA", xlab = "FRAGRANCE/AROMA")
# The distribution central tendencies of the FLAVOR using box plot
boxplot(Coffee_data$FLAVOR, main = "FLAVOR", xlab = "FLAVOR")
# The distribution central tendencies of the SALT/ACID using box plot
boxplot(Coffee_data$"SALT/ ACID", main = "SALT/ACID", xlab = "SALT/ACID")
# The distribution central tendencies of the BITTER/SWEET using box plot
boxplot(Coffee_data$"BITTER/ SWEET", main = "BITTER/SWEET", xlab = "BITTER/SWEET")
# The distribution central tendencies of the AFTERTASTE using box plot
boxplot(Coffee_data$AFTERTASTE, main = "AFTERTASTE", xlab = "AFTERTASTE")
# The distribution central tendencies of the MOUTH FEEL using box plot
boxplot(Coffee_data$"MOUTH FEEL", main = "MOUTH FEEL", xlab = "MOUTH FEEL")
# The distribution central tendencies of the OVERALL using box plot
boxplot(Coffee_data$OVERALL, main = "OVERALL", xlab = "OVERALL")

##### part e #####
# Show which variables are normally distributed and which ones are skewed.
#Using the histogram to show the distribution of the variables
hist(Coffee_data$"FRAGRANCE/AROMA", main = "FRAGRANCE/AROMA", xlab = "FRAGRANCE/AROMA")
#Display statistical values to back up your resu
hist(Coffee_data$FLAVOR, main = "FLAVOR", xlab = "FLAVOR")
hist(Coffee_data$"SALT/ ACID", main = "SALT/ACID", xlab = "SALT/ACID")
hist(Coffee_data$"BITTER/ SWEET", main = "BITTER/SWEET", xlab = "BITTER/SWEET")
hist(Coffee_data$AFTERTASTE, main = "AFTERTASTE", xlab = "AFTERTASTE")
hist(Coffee_data$"MOUTH FEEL", main = "MOUTH FEEL", xlab = "MOUTH FEEL")
#the overrall 
hist(Coffee_data$OVERALL, main = "OVERALL", xlab = "OVERALL")


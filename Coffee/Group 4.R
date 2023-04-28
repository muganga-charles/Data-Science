library(readxl)

work <- read_excel(("D:/R/R/Group Assignment 1_Cup sensory results-CWDr lines.xlsx"))#assigning the imported excel file to work 
View(work)

library(ggplot2)

ORIGIN.Mukono <- work[work$ORIGIN == "Mukono",]

View(ORIGIN.Mukono)

ORIGIN.Ibanda <-work[work$ORIGIN == "Ibanda",]
View(ORIGIN.Ibanda)

ORIGIN.Mityana <-work[work$ORIGIN == "Mityana",]

View(ORIGIN.Mityana)

Mukono.overall <- sum(ORIGIN.Mukono$OVERALL)
Ibanda.overall <- sum(ORIGIN.Ibanda$OVERALL)
Mityana.overall <- sum(ORIGIN.Mityana$OVERALL)

# putting into a dataframe
ORIGIN.Overall_challeges <- c(Mukono.overall, Ibanda.overall, Mityana.overall)

ORIGINS<-c("Mityana", "Ibanda", "Mukono")
ORIGIN.Data <- data.frame(ORIGINS, ORIGIN.Overall_challeges)
View(ORIGIN.Data)# collection one

ORIGIN.ggplot<- ggplot(ORIGIN.Data, aes(ORIGINS, ORIGIN.Overall_challeges))
ORIGIN.ggplot + geom_col(fill = "skyblue" ) + labs(title = "ORIGINS against overall_challenges")# Graph one

####
ORIGIN.ggplot <- ggplot(data = ORIGIN.Data) +
  geom_point(mapping = aes(x = ORIGINS, y = ORIGIN.Overall_challeges), color = "red")

####

# aroma occurance in diffrent districts
Mukono.FRAGRANCE_AROMA <- sum(ORIGIN.Mukono$"FRAGRANCE/AROMA")
Ibanda.FRAGRANCE_AROMA <- sum(ORIGIN.Ibanda$"FRAGRANCE/AROMA")
Mityana.FRAGRANCE_AROMA  <- sum(ORIGIN.Mityana$"FRAGRANCE/AROMA")
ORIGIN.aroma<-c(Mukono.FRAGRANCE_AROMA , Ibanda.FRAGRANCE_AROMA , Mityana.FRAGRANCE_AROMA)

# FLAVOR occurance in diffrent districts
Mukono.FLAVOR <- sum(ORIGIN.Mukono$FLAVOR)
Ibanda.FLAVOR <- sum(ORIGIN.Ibanda$FLAVOR)
Mityana.FLAVOR <- sum(ORIGIN.Mityana$FLAVOR)
ORIGIN.Flavor<-c(Mukono.FLAVOR, Ibanda.FLAVOR, Mityana.FLAVOR)

# salt/Acid  occurence in diffrent districts
Mukono.SALT_ACID <- sum(ORIGIN.Mukono$"SALT/ ACID")
Ibanda.SALT_ACID <- sum(ORIGIN.Ibanda$"SALT/ ACID")
Mityana.SALT_ACID <- sum(ORIGIN.Mityana$"SALT/ ACID")
ORIGIN.salt_acid<-c(Mukono.SALT_ACID, Ibanda.SALT_ACID, Mityana.SALT_ACID)

# BITTER/SWEET  occurence in diffrent districts
Mukono.BITTER_SWEET  <- sum(ORIGIN.Mukono$"BITTER/ SWEET")
Ibanda.BITTER_SWEET  <- sum(ORIGIN.Ibanda$"BITTER/ SWEET")
Mityana.BITTER_SWEET  <- sum(ORIGIN.Mityana$"BITTER/ SWEET")
ORIGIN.bitter_sweet<-c(Mukono.BITTER_SWEET, Ibanda.BITTER_SWEET, Mityana.BITTER_SWEET)

# AFTERTASTE  occurence in diffrent districts
Mukono.AFTERTASTE  <- sum(ORIGIN.Mukono$AFTERTASTE)
Ibanda.AFTERTASTE <- sum(ORIGIN.Ibanda$AFTERTASTE)
Mityana.AFTERTASTE <- sum(ORIGIN.Mityana$AFTERTASTE)
ORIGIN.AFTERTASTE<-c(Mukono.AFTERTASTE, Ibanda.AFTERTASTE, Mityana.AFTERTASTE)

# MOUTH FEEL  occurence in diffrent districts
Mukono.MOUTHFEEL  <- sum(ORIGIN.Mukono$"MOUTH FEEL")
Ibanda.MOUTHFEEL <- sum(ORIGIN.Ibanda$"MOUTH FEEL")
Mityana.MOUTHFEEL <- sum(ORIGIN.Mityana$"MOUTH FEEL")
ORIGIN.MOUTHFEEL<-c(Mukono.MOUTHFEEL, Ibanda.MOUTHFEEL, Mityana.MOUTHFEEL)

# BALANCE  occurence in diffrent districts
Mukono.BALANCE  <- sum(ORIGIN.Mukono$BALANCE)
Ibanda.BALANCE <- sum(ORIGIN.Ibanda$BALANCE)
Mityana.BALANCE <- sum(ORIGIN.Mityana$BALANCE)
ORIGIN.BALANCE<-c(Mukono.BALANCE,Ibanda.BALANCE,Mityana.BALANCE)

# UNIFORMITY  occurence in diffrent districts
Mukono.UNIFORMITY  <- sum(ORIGIN.Mukono$UNIFORMITY)
Ibanda.UNIFORMITY <- sum(ORIGIN.Ibanda$UNIFORMITY)
Mityana.UNIFORMITY <- sum(ORIGIN.Mityana$UNIFORMITY)
ORIGIN.UNIFORMITY<-c(Mukono.UNIFORMITY,Ibanda.UNIFORMITY,Mityana.UNIFORMITY)

# CLEAN CUPS  occurence in diffrent districts
Mukono.CLEAN_CUPS  <- sum(ORIGIN.Mukono$"CLEAN CUPS")
Ibanda.CLEAN_CUPS <- sum(ORIGIN.Ibanda$"CLEAN CUPS")
Mityana.CLEAN_CUPS <- sum(ORIGIN.Mityana$"CLEAN CUPS")
ORIGIN.CLEAN_CUPS<-c(Mukono.CLEAN_CUPS,Ibanda.CLEAN_CUPS,Mityana.CLEAN_CUPS)


# challenges for mukono
Challenges <- c("FRAGRANCE/AROMA", "FLAVOR", "SALT/ ACID","BITTER/ SWEET","AFTERTASTE","MOUTH FEEL","BALANCE","UNIFORMITY","CLEAN CUPS")
Mukono.Challenges <- c(Mukono.FRAGRANCE_AROMA,Mukono.FLAVOR,Mukono.SALT_ACID,Mukono.BITTER_SWEET,Mukono.AFTERTASTE,Mukono.MOUTHFEEL,Mukono.BALANCE,Mukono.UNIFORMITY,Mukono.CLEAN_CUPS)

Challenges.Data1<- data.frame(Challenges,Mukono.Challenges)
View(Challenges.Data1)#collection two

Challenges.ggplot<- ggplot(Challenges.Data1, aes(Challenges,Mukono.Challenges))
Challenges.ggplot + geom_col(fill = "skyblue" ) + labs(title = "Mukono Challenges")#Graph two 

#challenges for ibanda
Ibanda.Challenges <- c(Ibanda.FRAGRANCE_AROMA,Ibanda.FLAVOR,Ibanda.SALT_ACID,Ibanda.BITTER_SWEET,Ibanda.AFTERTASTE,Ibanda.MOUTHFEEL,Ibanda.BALANCE,Ibanda.UNIFORMITY,Ibanda.CLEAN_CUPS)

Challenges.Data2<- data.frame(Challenges,Ibanda.Challenges)
View(Challenges.Data2)#collection three

Challenges.ggplot<- ggplot(Challenges.Data1, aes(Challenges,Ibanda.Challenges))
Challenges.ggplot + geom_col(fill = "#ebcf87" ) + labs(title = "Ibanda Challenges")#Graph three

#challenges for Mityana
Mityana.Challenges <- c(Mityana.FRAGRANCE_AROMA,Mityana.FLAVOR,Mityana.SALT_ACID,Mityana.BITTER_SWEET,Mityana.AFTERTASTE,Mityana.MOUTHFEEL,Mityana.BALANCE,Mityana.UNIFORMITY,Mityana.CLEAN_CUPS)

Challenges.Data3<- data.frame(Challenges,Mityana.Challenges)
View(Challenges.Data3)#Collection 4

Challenges.ggplot<- ggplot(Challenges.Data1, aes(Challenges,Mityana.Challenges))
Challenges.ggplot + geom_col(fill = "#eb87d7" ) + labs(title = "Mityana Challenges")# Graph four

###################### variety###########

# categorising the diffrent varieties of coffee 
VARIETY.KR3 <- work[work$VARIETY == "KR3",]
View(VARIETY.KR3)
VARIETY.KR4 <- work[work$VARIETY == "KR4",]
View(VARIETY.KR4)
VARIETY.KR5 <- work[work$VARIETY == "KR5",]
View(VARIETY.KR5)
VARIETY.KR6 <- work[work$VARIETY == "KR6",]
View(VARIETY.KR6)
VARIETY.KR7 <- work[work$VARIETY == "KR7",]
View(VARIETY.KR7)

#counting the diffrent varieties found all the districts
KRE3.overall <- sum(VARIETY.KR3$OVERALL)
KRE4.overall <- sum(VARIETY.KR4$OVERALL)
KRE5.overall <- sum(VARIETY.KR5$OVERALL)
KRE6.overall <- sum(VARIETY.KR6$OVERALL)
KRE7.overall <- sum(VARIETY.KR7$OVERALL)

#putting them into a dataframe
Varieties.Overall_varieties <- c(KRE3.overall,KRE4.overall,KRE5.overall,KRE6.overall,KRE7.overall)
VARIETIES <- c("KRE3","KRE4","KRE5","KRE6","KRE7")
VARIETIES.DATA <- data.frame(VARIETIES,Varieties.Overall_varieties)
View(VARIETIES.DATA)# collection five
VARIETIES.ggplot <- ggplot(VARIETIES.DATA, aes(VARIETIES,Varieties.Overall_varieties))
VARIETIES.ggplot + geom_col(fill = "#ec63a5" ) + labs(title = "VARIETY against overall_Varieties")# Graph one
# addding up to find the total of the over all


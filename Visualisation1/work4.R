################# COPY OF SALES DATA ANALYSIS ##########################
# first we load in the data we are to study in this case 
# the copy of sales data 
# which is an excel file so we load in readxl package.
library(readxl)
Sales_data <- read_excel("D:/R/R/Sales data.xlsx")
View(Sales_data)

# We load in ggplot2 package to help with the data 
# visualization such that we can
# analyze the data.
library(ggplot2)
##Qstn : how, many pencils where sold

# To find the number of pencils first we create a smaller data set from copy of
# sales containing only pencils.
Pencils<-Sales_data[Sales_data$Item == "Pencil",]
View(Pencils)

# We add up all the pencils using the sum() function where we find that the total
# sales are 716.
Totalpencils <-sum(Pencils$Units)
View(Totalpencils)
Totalpencils

##QSTN : Compare the sales of the representatives usins a column graph

# For the next question we use a column graph to 
# compare the sales of each 
# Rep and from the code below we can see it is Andrew.
Sales<-ggplot(Sales_data, aes(Rep, Total))# Rep and TOtal are from the execell
Sales + geom_col(fill = "red") + labs(title = "REP SALES")
# from the graph, Adrews has the lowest sales.

#QSTN3: Find the total revenue of the binders.
# To find the total revenue of binders we create a smaller table for binders
# to analyze it.
Binders<- Sales_data[Sales_data$Item == "Binder",]
View(Binders)

# We add up the rev column to find the total binder revenue.
#Total.Binder.Rev<-sum(Binders$Total)
#View(Total.Binder.Rev)
#Total.Binder.Rev
TotalBinderRevenue <- sum(Binders$Total)
View(TotalBinderRevenue)

# We create tables focusing on each region to study it.
Region.East<-Sales_data[Sales_data$Region == "East",]
View(Region.East)
Region.West<-Sales_data[Sales_data$Region == "West",]
View(Region.West)
Region.Central<-Sales_data[Sales_data$Region == "Central",]
View(Region.Central)

# We add up the total revenue from each of the tables 
# for comparision
East.sales<-sum(Region.East$Total)
West.sales<-sum(Region.West$Total)
Central.sales<-sum(Region.Central$Total)

# We then create a data frame to help compare the total revenue for each region
Region.sales<-c(East.sales, Central.sales, West.sales)
Regions<-c("East", "Central", "West")
Region.Data<- data.frame(Regions, Region.sales)
View(Region.Data)

# We can continue to use graphics to further show comparison
Region.ggplot<- ggplot(Region.Data, aes(Regions, Region.sales))
Region.ggplot + geom_col(fill = "skyblue" ) + labs(title = "REGION SALES")

# For finding the representative to get the top notch and the number of
# representatives per region we run the codes below.
East.gplot<-ggplot(Region.East, aes(Rep, Total))
East.gplot + geom_col(fill = "green")+theme_bw()+labs(title = "East sales")
West.gplot<- ggplot(Region.West, aes(Rep, Total))
West.gplot + geom_col(fill = "yellow")+theme_bw()+labs(title = "West sales")
Central.gplot<-ggplot(Region.Central, aes(Rep, Total))
Central.gplot + geom_col(fill = "red")+theme_bw()+labs(title = "Central sales")

################### END #######################################
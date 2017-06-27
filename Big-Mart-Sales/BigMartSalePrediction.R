# SETTING DIRECTORY
setwd("E:/Data Science/R_Programs/Analytics Vidya")

# LOADING THE DATA SET
library(readr)
trainData <- read_csv("BigMartTrainData.csv")
testData <- read_csv("BigMartTestData.csv")

# ADDING Item_Outlet_Sales COLUMN IN THE TEST DATA
testData$Item_Outlet_Sales <- NA

# COMBINING DATA SET INTO ONE

bigMart <- rbind(trainData, testData)

# ANALYING THE DATA STRUCTURE, PREDICTOR AND TARGET VARIABLE
str(bigMart)

View(head(bigMart))

summary(bigMart)

unique(bigMart$Item_Fat_Content)

#CONVERTING LOWFAT AND FAT INTO FACTORS

library(plyr)

bigMart$Item_Fat_Content <- revalue(bigMart$Item_Fat_Content, replace = c("low fat" = "Low Fat", "LF" = "Low Fat","reg" = "Regular"))

bigMart$Item_Fat_Content <- as.factor(bigMart$Item_Fat_Content)




bigMart$Item_Fat_Content <- as.factor(bigMart$Item_Fat_Content)

# Item type
head(bigMart$Item_Type)
unique(bigMart$Item_Type)
bigMart$Item_Type <- as.factor(bigMart$Item_Type)
levels(bigMart$Item_Type)

# CHANIN OUTLET SIZE AND OUTLET LOCATION AND OUTLET TYPE INTO FACTORS
unique(bigMart$Outlet_Size)
bigMart$Outlet_Size <- as.factor(bigMart$Outlet_Size)

unique(bigMart$Outlet_Location_Type)
bigMart$Outlet_Location_Type <- as.factor(bigMart$Outlet_Location_Type)

unique(bigMart$Outlet_Type)
bigMart$Outlet_Type <- as.factor(bigMart$Outlet_Type)

# TOTAL MISSING VALUES
colSums(sapply(bigMart, is.na)) 

#MISSING VALUES IN ITEM WEIGHT, OUTLET SIZE

# ANALYZING ITEM WEIGHT MISSING VALUES
head(bigMart[which(is.na(bigMart$Item_Weight)),"Item_Identifier"])

library(ggplot2)

# MEAN ITEM WEIHT vS ITEM TYPE
ggplot(aes(x = Item_Type, y = Item_Weight), data = bigMart)+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Mean Item Weigth Vs Item Type")

# BOXPLOT OF ITEM WEIGHT WITH RESPECT TO THE  THE ITEM TYPE
ggplot(aes(x = Item_Type, y = Item_Weight), data = bigMart)+
  geom_boxplot()+
  ggtitle("Item Weigth Vs Item Type")

# FAT CONTENT VS ITEM WEIGHT AND ITEMTYPE
ggplot(aes(x = Item_Type, y = Item_Weight, fill = Item_Fat_Content), data = bigMart)+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  ggtitle("Mean Item Weigth Vs Item Type")

View(unique(bigMart[which(is.na(bigMart$Item_Weight)), "Item_Identifier"]))

# Filling Item Weight From Others Stores # 13.35

bigMart[which(bigMart$Item_Identifier == "FDN48"), c("Item_Identifier","Item_Fat_Content",
                                                     "Item_Weight")]

# Fiiling All The Null Values In Item Weight

library(dplyr)

byItem <- bigMart %>%
  filter(!is.na(bigMart$Item_Weight))%>%
  group_by(Item_Identifier) %>%
  summarise(ItemWeight = mean(Item_Weight))

for(i in which(is.na(bigMart$Item_Weight))){
  itmName = as.character(bigMart[i,"Item_Identifier"])
  bigMart[i,"Item_Weight"] = as.numeric(
    byItem[which(byItem$Item_Identifier == itmName),"ItemWeight"])
}

# Treating outlet SIZE missing values
sum(is.na(bigMart$Outlet_Size))

table(bigMart$Outlet_Size, bigMart$Outlet_Type, bigMart$Outlet_Location_Type)

# TIER 2 LOCATION HAS ONLY SUPER MARKET OF TYPE 1

# ALL THE GROCERY HAVE SMALL OUTLET SIZE

bigMart[which(is.na(bigMart$Outlet_Size) & bigMart$Outlet_Type == "Grocery Store"),
        "Outlet_Size"] <- "Small"

# sUPER mARKET TYPE ACC TO YEAR EST.
length(unique(bigMart$Outlet_Establishment_Year))
ggplot(aes(x = factor(Outlet_Establishment_Year), fill = Outlet_Size), data = bigMart)+
  geom_bar()

# AFTER YEAR 2000 ALL ARE EITHER SMALL OR MEDIUM


# HOW MANY NULL VALUES ARE FROM TIER 2 CITY
length(which(is.na(bigMart$Outlet_Size) & bigMart$Outlet_Location_Type == "Tier 2"))

unique(bigMart[which(is.na(bigMart$Outlet_Size) & bigMart$Outlet_Location_Type == "Tier 2"),"Outlet_Type"])

# Fiiling All the tier 2 city of type SuperMarket type 1 with small
bigMart[which(is.na(bigMart$Outlet_Size) & 
                bigMart$Outlet_Location_Type == "Tier 2"),"Outlet_Size"] <- "Small"


# Analyze the variables
View(head(bigMart))

# More Visible the Item , More will be the Sale , 0 star
ggplot(aes(x = Item_Outlet_Sales, y = Item_Visibility), 
       data = bigMart[1:8523,])+
  geom_point()+
  facet_wrap(~Outlet_Type)

# More Visible the Item , More will be the Sale , 0 star
ggplot(aes(x = Item_Outlet_Sales, y = Item_MRP), 
       data = bigMart[1:8523,])+
  geom_point()

# Item location , More will be the Sale , 0 star
ggplot(aes(x = Item_Sold, y = Item_Weight), data = bigMart[1:8523,])+
  geom_point()


# Creating Item Sold
trainData$Item_Sold <- trainData$Item_Outlet_Sales/trainData$Item_MRP

bigMart$Item_Sold <- rep(0)

bigMart[1:8523,"Item_Sold"] <- round(bigMart[1:8523,"Item_Outlet_Sales"]/
                                       bigMart[1:8523,"Item_MRP"],0)


# 
# More Visible the Item , More will be the Sale , 0 star
ggplot(aes(x = Item_Sold),
       data = bigMart[1:8523,])+
  geom_bar()+
  facet_wrap(~Outlet_Type)

ggplot(aes(x = Item_Sold),
       data = bigMart[1:8523,])+
  geom_bar()+
  facet_wrap(~Outlet_Type)

# CREATING THE No OF YEAR FOR THE OUTLET
bigMart$Year <- 2013 - bigMart$Outlet_Establishment_Year

# REmoving outlet established year
bigMart$Outlet_Establishment_Year <- NULL


# # using Linear regression
# 
lm.fit <- lm(Item_Sold ~ Outlet_Type+Outlet_Location_Type+Outlet_Size+
               Year, data = bigMart[1:8523,])

lm.predict <- predict(lm.fit, bigMart[8524:14204,])
lm.predictSales <- lm.predict*as.numeric(unlist(bigMart[8524:14204, "Item_MRP"]))

# Writing CSV file
write.csv(file = "BigMartSubmissionFile26_06_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = lm.predictSales),
          row.names = FALSE)

summary(lm.fit)

library(car)
vif(lm.fit)



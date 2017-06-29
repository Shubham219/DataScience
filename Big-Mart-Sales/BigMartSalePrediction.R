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
  theme(axis.text.x = element_text(angle = 70, vjust = 0.75))+
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

#Total no of Products available
length(unique(bigMart$Item_Identifier))

# Item Having Low Fate Should be sold More? 
ggplot(aes(x = Item_Fat_Content, y = Item_Outlet_Sales), data = bigMart[1:8523,])+
  geom_boxplot()

# More Visible the Item , More will be the Sale? 
ggplot(aes(x = Item_Outlet_Sales, y = Item_Visibility, color = Outlet_Type), 
       data = bigMart[1:8523,])+
  geom_point()

# boxplot of Visibility vs. Outlet Identifier
ggplot(aes(Outlet_Identifier, Item_Visibility), data = bigMart) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

# Item Visiblity doesnt have any NA values but some of the Item Visiblity records are
# 0 
visiblitysum <- bigMart %>%
  group_by(Outlet_Identifier) %>%
  summarise(Visiblity = sum(Item_Visibility))

visiblitysum

# For creating plot of missing data
library(VIM)

# Library for imputing NA Values 
library(mice)

# Let First Create all the 0 as NA

bigMart[which(bigMart$Item_Visibility == 0), "Item_Visibility"] <- NA

summary(bigMart$Item_Visibility)

#Removing Item Outlet Sales
bigMart.miss <- select(bigMart, -c(Item_Outlet_Sales))

# Plot which shows the NA values
mice_plot <- aggr(bigMart.miss, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(bigMart.miss), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# Using mice packae to impute missing values

imputedData <- mice(bigMart.miss, m = 3, maxit = 10, method = "pmm", seed = 500)

summary(imputedData)

# check imputed data
imputedData$imp$Item_Visibility

# Select any one data set from 5
completeData <- complete(imputedData,2)

# Taking Average of the five data sets
missingData <- imputedData$imp$Item_Visibility

missingData$avg <- rowMeans(missingData)
head(missingData)

# Imputing Missing Value
bigMart[which(is.na(bigMart$Item_Visibility)), "Item_Visibility"] <- missingData$avg

summary(bigMart$Item_Visibility)

# Sum of Visiblities after imputing
visiSum <- bigMart %>%
  group_by(Outlet_Identifier) %>%
  summarise(Visiblity = sum(Item_Visibility))

# let's normalize all visibilities such that
# the total per shop comes out at 100
str(bigMart$Outlet_Identifier)
bigMart$Outlet_Identifier <- as.factor(bigMart$Outlet_Identifier)

for (outName in levels(bigMart$Outlet_Identifier)) {
  bigMart[which(bigMart$Outlet_Identifier == outName),]$Item_Visibility <-
    bigMart[which(bigMart$Outlet_Identifier == outName),]$Item_Visibility *
    100/visiSum[which(visiSum$Outlet_Identifier == outName),]$Visiblity
}

# Checking Visibility again
bigMart %>%
  group_by(Outlet_Identifier) %>%
  summarise(Visiblity = sum(Item_Visibility))

head(bigMart$Item_Visibility)

# Density curve of the visiblies after imputing
ggplot()+
  geom_density(aes(x = Item_Visibility), data = bigMart, colour = "blue")

# More Visible the Item , More will be the Sale , 0 star
ggplot(aes(x = Item_Outlet_Sales, y = Item_Visibility, color = Item_Type), 
       data = bigMart[1:8523,])+
  geom_point()+
  facet_wrap(~Outlet_Type)

# Item type vs the Item _outlet_sales
ggplot(aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type), data = bigMart[1:8253,])+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = "blue"))+
  ggtitle("Item Type Vs Item Sale")

# Creating Item Sold
trainData$Item_Sold <- trainData$Item_Outlet_Sales/trainData$Item_MRP

bigMart$Item_Sold <- rep(0)

bigMart[1:8523,"Item_Sold"] <- round(bigMart[1:8523,"Item_Outlet_Sales"]/
                                       bigMart[1:8523,"Item_MRP"], 4)

# Itype type Vs Item_Sold
ggplot(aes(x = Item_Type, y = Item_Sold, fill = Outlet_Type), data = bigMart[1:8253,])+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, color = "blue"))+
  ggtitle("Item Type Vs Item Sold")

# Item Sold according to the city
ggplot(aes(x = Outlet_Location_Type, y = Item_Sold, fill = Outlet_Type), data = bigMart[1:8253,])+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, color = "blue"))+
  ggtitle("Item Type Vs Item Sold")

# Combining Level of SuperMarket type 1 and 2
# bigMart$Outlet_Type <- revalue(bigMart$Outlet_Type,
#                                replace = c("Supermarket Type2" = "Supermarket Type2",
#                                            "Supermarket Type1" = "Supermarket Type2"))
# 
# bigMart$Outlet_Type <- as.factor(bigMart$Outlet_Type)
# levels(bigMart$Outlet_Type)

# Doesnt have any effect plus we are lossing some info

# Item MRP density curve
ggplot()+
geom_density(aes(Item_MRP), data = bigMart, adjust = 0.5)+
  geom_vline(xintercept = 70, color = "blue" )+
  geom_vline(xintercept = 136, color = "blue")+
  geom_vline(xintercept = 205, color = "blue")+
  ggtitle("Density Curve Of Item MRP")

# We Divided the ITEM_MRP into 4 Parts low, medium, high, very high
bigMart$MRP_Level <- ifelse(bigMart$Item_MRP < 70,"LOW",
                                      ifelse(bigMart$Item_MRP < 136, "Medium",
                                             ifelse(bigMart$Item_MRP < 205,"High",
                                                    "Very High")))

bigMart$MRP_Level <- as.factor(bigMart$MRP_Level)
str(bigMart$MRP_Level)

# Extracting The Item identifier name
bigMart$Item_Class <- strtrim(bigMart$Item_Identifier, 2)

bigMart$Item_Class <- as.factor(bigMart$Item_Class)

# Arranging by item type
bigMart %>%
  group_by(Item_Class, Item_Type) %>%
  summarise(Total = n())

levels(bigMart$Item_Class)

# CREATING THE No OF YEAR FOR THE OUTLET
bigMart$Year <- 2013 - bigMart$Outlet_Establishment_Year

# REmoving outlet established year
bigMart$Outlet_Establishment_Year <- NULL

# Checking the correlation between numerical variables

# correlation between numerical variables
corMatrix <- cor(bigMart[1:8523,][sapply(bigMart[1:8523,], is.numeric)])
corMatrix

# plotting correlation Matrix
library(corrplot)
corrplot(corMatrix, type = "upper", method = "number")

# Item MRP has strong corelation with Sales

# Lets Do Some Feature Selection
names(bigMart)
str(bigMart)

# doing it on parallel cores
library(doParallel)
cl <- makeCluster(detectCores()); 
registerDoParallel(cl)

library(Boruta)
set.seed(100)
boruta.train <- Boruta(Item_Sold~.-Item_Outlet_Sales, data = bigMart[1:8523,], doTrace = 2)

stopCluster(cl); 
registerDoSEQ();

print(boruta.train)

par(las = 2)
plot(boruta.train, cex.axis = 0.6)

getSelectedAttributes(boruta.train, withTentative = FALSE)

boruta.df <- attStats(boruta.train)
boruta.df

# # using Linear regression
# 
# # Taking only )UT17 into consideration
# bigMart$OUT <- ifelse(bigMart$Outlet_Identifier == "OUT017", 1, 0)
# table(bigMart$OUT)
# 
# lm.fit <- lm(Item_Sold ~ Outlet_Type+Outlet_Location_Type+Outlet_Size+
#                Year, data = bigMart[1:8523,])
# 
# lm.predict <- predict(lm.fit, bigMart[8524:14204,])
# lm.predictSales <- lm.predict*as.numeric(unlist(bigMart[8524:14204, "Item_MRP"]))
# 
# summary(lm.fit)
# 
# # Writing CSV file
# write.csv(file = "BigMartSubmissionFile30_06_17.csv",
#           x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
#                          Item_Outlet_Sales = lm.predictSales),
#           row.names = FALSE)


library(xgboost)

trainDat <- model.matrix(Item_Sold ~ Outlet_Type+Outlet_Location_Type+Outlet_Size+
                                    Year+MRP_Level+Outlet_Identifier,
                         data = bigMart[1:8523,])

y = unlist(bigMart[1:8523,"Item_Sold"])


testDat <- model.matrix(~ Outlet_Type+Outlet_Location_Type+Outlet_Size+
                          Year+MRP_Level+Outlet_Identifier,
                        data = bigMart[8524:14204,])

xgb <- xgboost(data = trainDat, 
               label = y, 
               nround=50,
               booster = "gblinear",
               objective = "reg:linear"
               
)


xgb.predict <- predict(xgb, testDat)
mrp <- as.vector(unlist(bigMart[8524:14204, "Item_MRP"]))
xgb.predictSales <- xgb.predict*mrp
head(xgb.predictSales)
# Writing CSV file
write.csv(file = "BigMartSubmissionFile1_06_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = xgb.predictSales),
          row.names = FALSE)


# library(randomForest)
# rf.fit <- randomForest(Item_Sold ~ Outlet_Type+Outlet_Location_Type+Outlet_Size+
#                         Year, data = bigMart[1:8523,],
#                        ntree = 500)
# 
# rf.fit
# 
# rf.predict <- predict(rf.fit, bigMart[8524:14204,])
# 
# summary(lm.fit)
# 
# # Writing CSV file
# write.csv(file = "BigMartSubmissionFile29_06_17.csv",
#           x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
#                          Item_Outlet_Sales = rf.predict),
#           row.names = FALSE)
# 
# varImpPlot(rf.fit)

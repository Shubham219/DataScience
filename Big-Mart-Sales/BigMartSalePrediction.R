# Clearing The Variables
rm(list = ls())

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

levels((bigMart$Item_Class))

# Assigning the fat content None to "NC Non COnsumable"
bigMart$Item_Fat_Content <- as.character(bigMart$Item_Fat_Content)

bigMart[which(bigMart$Item_Class == "NC"), "Item_Fat_Content"] <- "None"

bigMart$Item_Fat_Content <- as.factor(bigMart$Item_Fat_Content)


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

# Checking distribution of Item SOld
hist(bigMart$Item_Sold[1:8523])

library(e1071)

skewness(bigMart$Item_Sold[1:8523])

hist(log10(bigMart$Item_Sold[1:8523]))
skewness(log10(bigMart$Item_Sold[1:8523]))

hist(sqrt(bigMart$Item_Sold)[1:8523])
skewness(sqrt(bigMart$Item_Sold)[1:8523])

hist(((bigMart$Item_Sold)[1:8523]/max(bigMart$Item_Sold)))
skewness(((bigMart$Item_Sold)[1:8523]/max(bigMart$Item_Sold)))


# Sqrt is giving little bit better normal curve

# Minimun item Sold
min(round(bigMart$Item_Sold[1:8523],0))

# Is There ny relation with item solld and item weight
ggplot(aes(x = Item_Sold, y = Item_Weight), data = bigMart[1:8523,])+
  geom_point()+
  facet_wrap(~ Item_Class)
# Noting Usefull

#Item Sold Vs Fat Content
ggplot(aes(y = Item_Sold, x = Item_Fat_Content), data = bigMart[1:8523,])+
  geom_boxplot()+
  facet_wrap(~ Item_Class)

# Item Visibility Item SOld
ggplot(aes(y = Item_Sold, x = Item_Visibility), data = bigMart[1:8523,])+
  geom_point()+
  facet_wrap(~ Outlet_Identifier)

ggplot(aes(y = Item_Sold, x = Item_Visibility, col = Item_Class), data = bigMart[1:8523,])+
  geom_point()+
  facet_wrap(~ Outlet_Identifier)

# Item Visiiblity and food class 
ggplot(data = bigMart)+
  geom_density(aes(x = bigMart$Item_Visibility), adjust = 0.75)+
  facet_wrap(~ Outlet_Identifier)

# Checking THe Item_FatContent Vs Item Sold
ggplot(aes(x = Item_Fat_Content, y = Item_Sold), data = bigMart[1:8523,])+
  geom_boxplot() # Nothing Useful


head(bigMart$Item_Identifier)

head(bigMart$Outlet_Identifier)

bigMart[which(bigMart$Item_Identifier == "FDP36"), c("Item_Sold","Outlet_Identifier","Item_MRP")]

prouduct <- levels(as.factor(bigMart$Item_Identifier))

View(bigMart[1:8523,]%>%
  group_by(Outlet_Identifier, Item_Identifier) %>%
  summarise(ItemSold = mean(Item_Sold),
            Item_Visibility = mean(Item_Visibility)*100))

# Seems no effect of Visibilty on Item Sold

# Lets Do Some Feature Selection
names(bigMart)
str(bigMart)

# doing it on parallel cores
library(doParallel)

# cl <- makeCluster(detectCores()); 
# registerDoParallel(cl)
# 
# # Feature Selection Using Boruta 
# library(Boruta)
# set.seed(100)
# boruta.train <- Boruta(Item_Sold~.-Item_Outlet_Sales, data = bigMart[1:8523,], doTrace = 2)
# 
# stopCluster(cl); 
# registerDoSEQ();
# 
# print(boruta.train)
# 
# par(las = 2)
# plot(boruta.train, cex.axis = 0.6)
# 
# getSelectedAttributes(boruta.train, withTentative = FALSE)
# 
# boruta.df <- attStats(boruta.train)
# boruta.df

# Making Predictive Models
#==========================================================================================
# Linear regression model

# Preparing Data
lm.train <- select(bigMart[1:8523,], -c(Item_Weight,Item_Identifier,Item_Type, 
                                        Item_Outlet_Sales, Outlet_Identifier))


lm.test <- select(bigMart[8524:14204,], -c(Item_Weight,Item_Outlet_Sales,Item_Type,
                                           Item_Identifier, Outlet_Identifier))


lm.fit <- lm(Item_Sold~., data = lm.train)

summary(step(lm(Item_Sold ~., data = lm.train), direction = "backward", trace = 2))

lm.fit <- lm(formula = Item_Sold ~ Item_MRP + Outlet_Size + Outlet_Location_Type + 
     Outlet_Type + Year + I(Item_Visibility*100), data = lm.train)


summary(lm.fit)

# MOst of the variables are correlated with each other
# All outlet Variables are Coreleated with Each Other

# Using Chisq test Excluding Outlet Type
chisq.test(lm.train$Outlet_Location_Type, lm.train$Outlet_Type)

chisq.test(lm.train$Outlet_Location_Type, lm.train$Outlet_Size)

chisq.test(lm.train$Outlet_Location_Type, lm.train$Outlet_Location_Type)


# Using only outlet identifier for training

lm.fit1 <- lm(Item_Sold ~ Outlet_Location_Type+Outlet_Size+Outlet_Type+Year,
              data = lm.train)

summary(lm.fit1)

# 10 fold cross validation
library(caret)

cv <- caret::train(Item_Sold ~ Outlet_Location_Type+Outlet_Size+Outlet_Type+Year,
            data = lm.train, 
            method = "lm", 
            trControl = trainControl(method = "repeatedcv", repeats = 10))
cv

# Making Predictons
lm.predict <- predict(lm.fit1, newdata = lm.test)

lm.salesPrice <- lm.predict * lm.test$Item_MRP

# Writing Submission Files
write.csv(file = "BigMartSubmissionFile03_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = lm.salesPrice),
          row.names = FALSE)

# Leader Board Score 1148.195
#=======================================================================================================

# Preparing Data For glmnet
x.train <-model.matrix(Item_Sold ~ Outlet_Location_Type+Outlet_Size+Outlet_Type+Year,
                       data = lm.train)[,-1]
y.train <- lm.train$Item_Sold

x.test <- model.matrix(Item_Sold ~ Outlet_Location_Type+Outlet_Size+Outlet_Type+Year,
                       data = lm.test)[,-1]


library(glmnet)
# Using cross validation for finding out suitable value of lambda
cv.lasso <- cv.glmnet(x = x.train, y = y.train, nfolds = 10, alpha = 1, 
                      lambda = 10^seq(10,-2, length = 1000))

#cv.lasso$cvm

cv.lasso$lambda.min

cv.lasso$lambda.1se

# Training the lasso model

lasso.fit <- glmnet(x = x.train, y = y.train, alpha = 1, 
                    lambda = cv.lasso$lambda.min)

plot(lasso.fit)

# Predicting coefficient 
predict(lasso.fit, s = cv.lasso$lambda.min, type = "coefficients")

# Predicting Sales
lasso.predict <- predict(lasso.fit, newx = x.test)

head(lasso.predict)

lasso.sales <- lm.test$Item_MRP*lasso.predict[,"s0"]

# Writing CSV file
write.csv(file = "BigMartSubmissionFile05_06_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = lasso.sales),
          row.names = FALSE)

# Leader Score Board 1149

# Leader Score Board With only outlet identifier var 1148.87
#=======================================================================================================

# Using Ridge Regression

cv.train <- cv.glmnet(x = x.train, y = y.train, lambda = 10^seq(10,-2, length = 100),
                      nfolds = 10, alpha = 0)

cv.train$nzero

# Mean Cross Validation error
cv.train$cvm

# Value of the lambda that gives min mean cv error
cv.train$lambda.min

# largest value of lambda such that error is within 1 standard error of the minimum
cv.train$lambda.1se

plot(cv.train)

# Training data on Ridge regression

# ridge.fit <- glmnet(x = x.train, y = y.train, alpha = 0, 
#                     lambda = cv.train$lambda.1se)
# Score boad 1157.1197

ridge.fit <- glmnet(x = x.train, y = y.train, alpha = 0, 
                    lambda = cv.train$lambda.min)

plot(ridge.fit)

ridge.predict <- predict(ridge.fit, newx = x.test)

ridge.sales <- x.test[,"Item_MRP"]*ridge.predict[,"s0"]

# Writing CSV file
write.csv(file = "BigMartSubmissionFile05_06_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = ridge.sales),
          row.names = FALSE)

# Leader Board Score 1149.758

#=======================================================================================================

# Using XG Boost For model boosting

library(xgboost)

# Preparing Data

xgbTrainData <- model.matrix(Item_Sold ~., 
                             data = bigMart[1:8523,c(4,6,7,8,9,10,12,14,15)])[,-1]

y = bigMart$Item_Sold[1:8523]


xgbTestData <- model.matrix(Item_Sold~., 
                            bigMart[8524:14204,c(4,6,7,8,9,10,12,14,15)])[,-1]

# Cross Validation in cv
params <- list(booster = "gblinear", objective = "reg:linear",
               gamma=0, max_depth=15, min_child_weight=1, subsample=0.8,
               colsample_bytree=0.5, seed = 1)


  cv <- xgb.cv(params = params, data = xgbTrainData,
             nrounds = 200, nfold = 10, label = y)

# Predicting Sales

xgb <- xgboost(params = params, data = xgbTrainData, nrounds = 200, label = y)

xgb.predict <- predict(xgb, xgbTestData)

mrp <- as.vector(unlist(bigMart[8524:14204, "Item_MRP"]))

xgb.predictSales <- xgb.predict * mrp

# Writing CSV file
write.csv(file = "BigMartSubmissionFile09_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = xgb.predictSales),
          row.names = FALSE)



# Using MLRn

library(mlr)

mlrTrainData <- as.data.frame(bigMart[1:8523,c(4,6,7,8,9,10,12,14,15)])

mlrTestData <- as.data.frame(bigMart[8524:14204,c(4,6,7,8,9,10,12,14,15)])

#create tasks
traintask <- makeRegrTask(data = mlrTrainData, target = "Item_Sold")
testtask <- makeRegrTask(data = mlrTestData,target = "Item_Sold")

#do one hot encoding`<br/> 
traintask <- createDummyFeatures(obj = traintask) 
testtask <- createDummyFeatures(obj = testtask)

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list(objective="reg:linear", eval_metric="rmse", 
                      nrounds=500L, eta = 0.1)

#set parameter space
params <- makeParamSet(makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                       makeIntegerParam("max_depth",lower = 3L,upper = 10L),
                      
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",iters=5L)

ctrl <- makeTuneControlRandom(maxit = 10L)

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc,
                     measures = rmse, par.set = params, control = ctrl, show.info = T)

mytune$y 

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

xg.salesPrice <- xgpred$data$response * bigMart$Item_MRP[8524:14204]

# Writing CSV file
write.csv(file = "BigMartSubmissionFile09_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = xg.salesPrice),
          row.names = FALSE)

# Leader Board Score 1148.73

#================================================================================
library(h2o)

# Load Datasets with the new predictors
# in the order of their importance,
# according to rfe
# predictors <- lm.train[,c("Outlet_Location_Type", "Outlet_Size", "Outlet_Type",
#                           "Year","Item_Sold")]

# Initiating h2o
localH2O <- h2o.init(nthreads = -1)

# Training the parameter
predictors <- bigMart[1:8523,c(4,6,8,9,10,12,14,15)]
train.h2o <- as.h2o(predictors)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = train.h2o, 
                         ratios = 0.8,  #partition data into 70%, 20% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility

# Preparing Data
h2o.trainData <- splits[[1]]

h2o.validationData <- splits[[2]]

h2o.testData <- as.h2o(bigMart[8524:14204,c(4,6,8,9,10,12,14,15)])

# Identify response and predictor variables
y <- "Item_Sold"
x <- c(1:5,7,8)




#==============================================================================================
# MUltiple regression model

regression.model <- h2o.glm(y = y, x = x, training_frame = train.h2o,
                             family = "gaussian")

h2o.performance(regression.model)

#make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model, h2o.testData))

sales <- predict.reg$predict * bigMart$Item_MRP[8524:14204]

write.csv(file = "BigMartSubmissionFile06_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = sales),
          row.names = FALSE)

#=================================================================================================

#Random Forest

# Top algo
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# system.time(
#   rforest.model <- h2o.randomForest(y=y.dep, x=c(1:5,7,8),
#                                     training_frame = train.h2o, 
#                                     ntrees = 2000, mtries = 6,
#                                     max_depth = 5, seed = 1122)
# )

# Finding Out Best Parameters For Random Forest 

# Setting Hyperparameters
rf_params <- list(mtries = seq(1,7,1),
                  max_depth = seq(3,10,1))

# Search Criteria
search_criteria <- list(strategy = "RandomDiscrete", 
                         max_models = 36)

# Ftting Parameters
rf_grid <- h2o.grid("randomForest", x=c(1:5,7,8), y = y,
                    grid_id = "rf_grid",
                    training_frame = h2o.trainData,
                    validation_frame = h2o.validationData,
                    ntrees = 500,
                    seed = 1,
                    hyper_params = rf_params,
                    search_criteria = search_criteria
)

rf_gridperf <- h2o.getGrid(grid_id = "rf_grid", 
                           sort_by = "rmse", 
                           decreasing = F)
print(rf_gridperf)


best_rf_model_id <- rf_gridperf@model_ids[[1]]
best_rf <- h2o.getModel(best_rf_model_id)

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_rf_perf <- h2o.performance(model = best_rf, 
                                newdata = h2o.testData)
h2o.rmse(best_rf_perf)  # 0.683855910541

predict.rf <- as.data.frame(h2o.predict(best_rf, h2o.testData))

sales <- predict.rf$predict * bigMart$Item_MRP[8524:14204]

write.csv(file = "BigMartSubmissionFile06_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = sales),
          row.names = FALSE)


system.time(
  rforest.model <- h2o.randomForest(y=y, x=x,
                                    training_frame = train.h2o,
                                    ntrees =  1500, mtries = 7,
                                    max_depth = 5, seed = 1122)
)



h2o.performance(rforest.model)

#check variable importance
h2o.varimp(rforest.model)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, h2o.testData))

sales <- predict.rforest$predict * bigMart$Item_MRP[8524:14204]

write.csv(file = "BigMartSubmissionFile06_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = sales),
          row.names = FALSE)

#=================================================================================================

#GBM
system.time(
  gbm.model <- h2o.gbm(y=y, x=x, training_frame = train.h2o,
                       ntrees = 1000, max_depth = 3, 
                       learn_rate = 0.01, seed = 1122,
                       col_sample_rate = 0.5,
                       sample_rate = 0.7)
)

h2o.performance (gbm.model)

predict.gbm <- as.data.frame(h2o.predict(gbm.model, h2o.testData))

sales <- predict.gbm$predict * bigMart$Item_MRP[8524:14204]

write.csv(file = "BigMartSubmissionFile19_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = sales),
          row.names = FALSE)

#=======================================================================================================================

nfolds <- 5
# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train.h2o,
                  max_depth = 3,
                  ntrees = 1000,
                  col_sample_rate = 0.5,
                  sample_rate = 0.8,
                  learn_rate = 0.01,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train.h2o,
                          nfolds = nfolds,
                          max_depth = 6,
                          mtries = 4,
                          ntrees = 1000,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

my_rf
# Train & Cross-validate a DNN

# Train regression model
my_lm <- h2o.glm(x = x, y = y, training_frame = train.h2o, family = "gaussian",
                 nfolds = nfolds, fold_assignment = "Modulo",
                 keep_cross_validation_predictions = TRUE,
                 seed = 1)

# Train a stacked ensemble using the H2O and XGBoost models from above
base_models <- list(my_gbm@model_id, my_rf@model_id, my_lm@model_id)

ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train.h2o,
                                base_models = base_models)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = train.h2o)


predict.ensm <- as.data.frame(h2o.predict(ensemble, h2o.testData))

sales <- predict.ensm$predict * bigMart$Item_MRP[8524:14204]

write.csv(file = "BigMartSubmissionFile17_07_17.csv",
          x = data.frame(bigMart[8524:14204, c("Item_Identifier","Outlet_Identifier")],
                         Item_Outlet_Sales = sales),
          row.names = FALSE)

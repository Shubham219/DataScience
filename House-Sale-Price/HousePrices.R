# Importing the data
houseSale1 <- read.csv(file = "~/Downloads/kaggleHouseSaleTrain.csv")

houseSale2 <- read.csv(file = "~/Downloads/kaggleHouseSaleTest.csv")

# Combining The Data
houseSale2$SalePrice <- NA 

houseSale <- rbind(houseSale1, houseSale2)

# Analyzing Variables
str(houseSale)

#Effect of the MS Subclass on the SalePrice
houseSale$MSSubClass <- as.factor(houseSale$MSSubClass)
library(ggplot2)

# Outlier in the sale price above 300000
ggplot(aes(x = 0, y = SalePrice), data = subset(houseSale, !is.na(houseSale$SalePrice)))+
  geom_boxplot()+
  ylim(c(0,300000))
  ggtitle("Sale Price Of The Houses")

houseSale1$MSSubClass <- as.factor(houseSale1$MSSubClass)

# MSSubClass VS Sale Price For Sales Price Less Than 300000
ggplot(aes(x = MSSubClass, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("MSSubclass Vs Sale Price")

# Comibining The Levels Based On  Influence On Sale Price

#Adding MSSubClass Star Variable in the data
houseSale$MSSubClassStar <- NA

for(i in 1:nrow(houseSale)){
  if(houseSale[i,"MSSubClass"] == "30" | 
     houseSale[i,"MSSubClass"] == "45" | 
     houseSale[i,"MSSubClass"] == "180") {
    
    houseSale[i,"MSSubClassStar"] = 1
  }
  else if (houseSale[i,"MSSubClass"] == "50" |
           houseSale[i,"MSSubClass"] == "85" |
           houseSale[i,"MSSubClass"] == "90" |
           houseSale[i,"MSSubClass"] == "160" |
           houseSale[i,"MSSubClass"] == "190"){
    
    houseSale[i,"MSSubClassStar"] = 2
  }
  else if(houseSale[i,"MSSubClass"] == "20" |
          houseSale[i,"MSSubClass"] == "40" |
          houseSale[i,"MSSubClass"] == "70" |
          houseSale[i,"MSSubClass"] == "75" |
          houseSale[i,"MSSubClass"] == "80"){
    
    houseSale[i,"MSSubClassStar"] = 3
  }
  else {
    houseSale[i,"MSSubClassStar"] = 4
  }
}

houseSale$MSSubClassStar <- as.factor(houseSale$MSSubClassStar)

# Plotting MSSubclass Star Vs The Sale Price on Sale Price less Than 300000
ggplot(aes(x = MSSubClassStar, y = SalePrice),
       data = subset(houseSale, houseSale$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean, width = 0.5)+
  ggtitle("MSSubclass Vs Sale Price")

# Plotting MSSubclass Star Vs The Sale Price on Sale Price Greater Than 30000
ggplot(aes(x = MSSubClassStar, y = SalePrice),
       data = subset(houseSale, houseSale$SalePrice > 300000))+
  geom_bar(stat = "summary", fun.y = mean, width = 0.5, fill = "lightblue")+
  ggtitle("MSSubclass Vs Sale Price")


# MSZoning VS Sale Price 
ggplot(aes(x = MSZoning, y = SalePrice),
               data = houseSale1)+
    geom_bar(stat = "summary", fun.y = mean, width = 0.5)+
    ggtitle("MSZoning Vs Sale Price Less Than 300000")

# Lot Frontage 
str(houseSale$LotFrontage)
summary(houseSale$LotFrontage)
unique(houseSale$LotFrontage)

ggplot(aes(x = factor(LotFrontage), y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Lot Frontage Vs Sale Price Less Than 300000")

#Lot Area 
str(houseSale$LotArea)
summary(houseSale$LotArea)

ggplot(aes(x = LotArea, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_point()+
  xlim(c(0,20000))
  ggtitle("Lot Area Vs Sale Price Less Than 300000")

# Street **
ggplot(aes(x = Street, y = SalePrice),
         data = houseSale1)+
    geom_bar(stat = "summary", fun.y = mean)+
    ggtitle("Street Vs Sale Price")

# Allet Acess to the property LESS THAN 300000 **
ggplot(aes(x = Alley, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("ALLEY Vs Sale Price Less Than 300000")

# Allet Acess to the property GREATER THAN 300000 ***
ggplot(aes(x = Alley, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice >= 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Alley Vs Sale Price Greater Than 300000")

# LOT SHAPE 
ggplot(aes(x = LotShape, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Lot Shape Lesser than 300000")

#combining Lot Shape Levels
houseSale$LotShapeStar <- NA
for( i in 1:nrow(houseSale)){
  if(houseSale[i,"LotShape"] == "Reg"){
    houseSale[i,"LotShapeStar"] <- "Reg"
  }
  else{
    houseSale[i,"LotShapeStar"] <- "IR"
  }
}
houseSale$LotShapeStar <- as.factor(houseSale$LotShapeStar)

#Land Countour and Sale Price less than 300k
ggplot(aes(x = LandContour, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice >= 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Lot Shape Lesser than 300000")

#Combing factor for HLS and LOW
houseSale$LandContourStar <- NA
for( i in 1:nrow(houseSale)){
  if(houseSale[i,"LandContour"] == "HLS" | houseSale[i,"LandContour"] == "Low"){
    houseSale[i,"LandContourStar"] <- "H&L"
  }
  else if (houseSale[i,"LandContour"] == "Bnk"){
    houseSale[i,"LandContourStar"] <- "Bnk"
  }
  else{
    houseSale[i,"LandContourStar"] <- "Lvl"
  }
}
houseSale$LandContourStar <- as.factor(houseSale$LandContourStar)

# Utilites Effect On Sale Price lesser than 300k, **
ggplot(aes(x = Utilities, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = median)+
  ggtitle("Utilites Lesser than 300000")

# Utilites Effect On Sale Price greater than 300k , No Effect
ggplot(aes(x = Utilities, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice >= 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Utilites Greater than 300000")

# Lot Config Effect On Sale Price Lesser Than 300k
ggplot(aes(x = LotConfig, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = median)+
  ggtitle("LotConfig Effect On Sale Less Than 300000")

# Land Slope
ggplot(aes(x = LandSlope, y = SalePrice),
       data = subset(houseSale1, houseSale1$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Utilites Greater than 300000")
table(houseSale$LandSlope)

# Combining Mod and Sev
houseSale$LandSlopeStar <- NA
for( i in 1:nrow(houseSale)){
  if(houseSale[i,"LandSlope"] == "Mod" | houseSale[i,"LandSlope"] == "Sev"){
    houseSale[i,"LandSlopeStar"] <- "MS"
  }
  else{
    houseSale[i,"LandSlopeStar"] <- "Gtl"
  }
}
houseSale$LandSlopeStar <- as.factor(houseSale$LandSlopeStar)


# Neighborhood: Physical locations within Ames city limits
ggplot(aes(x = Neighborhood, y = SalePrice),
       data = subset(houseSale, houseSale$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean)+
  ggtitle("Utilites Greater than 300000")

#Condition 1
ggplot(aes(x = Condition1, y = SalePrice),
       data = subset(houseSale, houseSale$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  ggtitle("Conditionns1 Effect on Sales Price Lesser than 300000")

# combining Levels
#combining the levels
houseSale$Condition1star <- NA
for( i in 1:nrow(houseSale)){
  if (houseSale1[i,"SalePrice"] < 300000){
    
      if(houseSale[i,"Condition1"] == "PosA" | houseSale[i,"Condition1"] == "PosN"){
        houseSale[i,"Condition1Star"] <- "Higher"
      }
      else if (houseSale[i,"Condition1"] == "Norm" | houseSale[i,"Condition1"] == "RRAn" |
             houseSale[i,"Condition1"] == "RRNe" | houseSale[i,"Condition1"] == "RRNn"){
             houseSale[i,"Condition1Star"] <- "Middle"
      }
        else{
          houseSale[i,"Condition1Star"] <- "Normal"
        }
  }
}
houseSale$LandSlopeStar <- as.factor(houseSale$LandSlopeStar)


#Condition 2
ggplot(aes(x = Condition2, y = SalePrice),
       data = subset(houseSale, houseSale$SalePrice < 300000))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  ggtitle("Utilites Greater than 300000")


#

which(colSums(is.na(houseSale)) > 0)

sort(colSums(sapply(houseSale[,which(colSums(is.na(houseSale)) > 0)], is.na)), decreasing = TRUE)

sd(houseSale$PoolArea, na.rm = TRUE)

unique(houseSale$Neighborhood)[!unique(houseSale$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]

nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)
head(nbrh.map)

# library(ggbiplot)
# library(gridExtra)
# 
# # corrplot is needed for correlation plots
# library(corrplot)
install.packages("ggbiplot")
install.packages("gridExtra")
install.packages("corrplot")
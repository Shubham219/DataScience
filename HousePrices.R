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


# MSZoning VS Sale price Less than 30000
ggplot(aes(x = MSZoning, y = SalePrice),
       data = houseSale1)+
  geom_bar(stat = "summary", fun.y = mean, width = 0.5)+
  ggtitle("MSZoning Vs Sale Price Less Than 300000")

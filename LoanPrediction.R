# Data Exploration
# Variable Identification    
library(readr)
#loanData <- read_csv("~/R_Programs/loanPrediction.csv")
loanData <- read.csv("~/Downloads/loanPrediction.csv",na.strings=c("","NA"))
str(loanData)   #Target Variable : Loan_Status "Y" or "N"

loanData$Gender <- as.factor(loanData$Gender)
loanData$Married <- as.factor(loanData$Married)
loanData$Dependents <- as.factor(loanData$Dependents)
loanData$Education <- as.factor(loanData$Education)
loanData$Self_Employed <- as.factor(loanData$Self_Employed)
loanData$Credit_History <- as.factor(loanData$Credit_History)
loanData$Property_Area <- as.factor(loanData$Property_Area)
loanData$Loan_Status <- as.factor(loanData$Loan_Status)
loanData$Loan_Amount_Term <- as.factor(loanData$Loan_Amount_Term)

#Loading Test File

#loanDataTestfile <- read_csv("C:/Users/Shubham/Downloads/loanDataTestfile.csv")
loanDataTestfile <- read.csv("~/Downloads/loanDataTestfile.csv",na.strings=c("","NA"))

loanDataTestfile$Gender <- as.factor(loanDataTestfile$Gender)
loanDataTestfile$Married <- as.factor(loanDataTestfile$Married)
loanDataTestfile$Dependents <- as.factor(loanDataTestfile$Dependents)
loanDataTestfile$Education <- as.factor(loanDataTestfile$Education)
loanDataTestfile$Self_Employed <- as.factor(loanDataTestfile$Self_Employed)
loanDataTestfile$Credit_History <- as.factor(loanDataTestfile$Credit_History)
loanDataTestfile$Property_Area <- as.factor(loanDataTestfile$Property_Area)
loanDataTestfile$Loan_Amount_Term <- as.factor(loanDataTestfile$Loan_Amount_Term)
loanDataTestfile$Loan_Status <- NA

# Combining the Data

combinedData <- rbind(loanData, loanDataTestfile)

# Univariate Analysis

library(ggplot2)
ggplot(aes(x = 0, y = ApplicantIncome), data = combinedData)+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 9000))# outliers above 9000

ggplot(aes(x = 0, y = CoapplicantIncome), data = combinedData)+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 5700)) # Outliers above 5700

# Vivariate Analysis

ggplot(aes(x = Credit_History, fill = Married, y = ApplicantIncome), data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  facet_wrap(~ Gender)

ggplot(aes(x = Credit_History, y = ApplicantIncome), data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = median)+
  facet_wrap(~Property_Area)+
  ggtitle("Credit History Vs Applicant Income Vs Property Area")


ggplot(aes(x = Credit_History, y = ApplicantIncome), data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = median)+
  facet_wrap(~Education)+
  ggtitle("Credit History Vs Applicant Income Vs Education")

# Missing Value treatment

summary(combinedData)

# Females having CoAppIncome 0 ,  don't have 3+ depedents
ggplot(aes(x = Gender, fill = Dependents, y = CoapplicantIncome),
       data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")

# HUge Difference in Mean of Coapplicant Income of Males and Females
ggplot(aes(x = Gender, y = CoapplicantIncome), data = na.omit(combinedData))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,5700))+
  ylim(c(0,5700))

ggplot(aes(x = Gender, fill = Dependents, y = CoapplicantIncome),
       data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  facet_wrap(~Married)


#manually Filling one Depedent NA
combinedData[which(combinedData$Loan_ID == "LP001769"),"Dependents"] <- "0"

# GROUP BY GENDER WITH SALARY LESS THAN 5700
attach(combinedData)
library(dplyr)

byGender <- na.omit(combinedData) %>%
  group_by(Gender,Dependents) %>%
  filter(CoapplicantIncome < 5700)%>%
  summarise(meanCoAppSalary = mean(CoapplicantIncome),
            medianCoAppSalary = median(CoapplicantIncome),
            maxSalary = max(CoapplicantIncome),
            users = n()) 
byGender

# GROUP BY GENDER WITH SALARY GREATER THAN 5700 BUT NO NA
byGender2 <- na.omit(combinedData) %>%
  group_by(Gender,Dependents) %>%
  filter(CoapplicantIncome >= 5700)%>%
  summarise(meanCoAppSalary = mean(CoapplicantIncome),
            medianCoAppSalary = median(CoapplicantIncome),
            maxSalary = max(CoapplicantIncome),
            users = n()) 
byGender2

# Filling The Values
for(i in which(is.na(Gender))){

  if (combinedData[i,"CoapplicantIncome"] <= 5700){
    
    if(combinedData[i,"CoapplicantIncome"] <= 1000){
      combinedData[i,"Gender"] <- "Female"
    }
    else {
      combinedData[i,"Gender"] <- "Male"
    }
  }
}
  

# Filling Married Null Values

combinedData[which(is.na(Married)),"TotalIncome"]

# CREATING TOTAL INCOME 
combinedData$TotalIncome <- combinedData$ApplicantIncome + combinedData$CoapplicantIncome

# Outliers in Total Income Above 12500
ggplot(aes(x = 0, y = TotalIncome), data = na.omit(combinedData))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,12500))

ggplot(aes(x = Married, fill = Dependents, y = ApplicantIncome), data = 
         na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  ggtitle("Married Vs Dependents Vs Mean Applicant Income")

# MARRIED Vs Toatl Income
ggplot(aes(x = Married,  y = TotalIncome, fill = Property_Area), data = 
         na.omit(subset(combinedData, TotalIncome <= 12500)))+
  geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  facet_wrap(~Gender)
  ggtitle("Married Vs Mean Total Income")

# MARRIED VS COAPPLICANT INCOME
ggplot(aes(x = Married, y = CoapplicantIncome), data = 
         na.omit(subset(combinedData, CoapplicantIncome <= 5700)))+
  geom_bar(stat = "summary", fun.y = mean)

byMarried <- na.omit(combinedData) %>%
  group_by(Married) %>%
  filter(CoapplicantIncome <= 5700) %>%
  summarise(meanCoappIncome = mean(CoapplicantIncome),
            medianCoappIncome = median(CoapplicantIncome),
            user = n())
byMarried

#Filling Missing Values
for(i in which(is.na(combinedData$Married))){
  
  if(combinedData[i,"CoapplicantIncome"] > 1514){
    combinedData[i,"Married"] <- "Yes"
  }
  else {
    combinedData[i,"Married"] <- "No"
  }
}


# FIlling dePEDENTS MISSING value

combinedData[which(is.na(Dependents)),]

ggplot(aes(x = Dependents, fill = Married, y = TotalIncome), 
       data = na.omit(combinedData))+
  geom_bar(position = "dodge", stat = "summary", fun.y = mean)+
  facet_wrap(~Gender)

byDependent <- na.omit(combinedData) %>%
  group_by(Dependents) %>%
  filter(ApplicantIncome <= 9000 ) %>%
  summarise(meanIncome = mean(ApplicantIncome),
            user = n())
byDependent

for(i in which(is.na(Dependents))){
  if(combinedData[i,"ApplicantIncome"] <= 3800){
    combinedData[i,"Dependents"] <- "0"
  }
  else if (combinedData[i,"ApplicantIncome"] <= 4100){
    combinedData[i,"Dependents"] <- "1"
  }
  else if(combinedData[i,"ApplicantIncome"] <= 4257){
    combinedData[i,"Dependents"] <- "2"
  }
  else{
    combinedData[i,"Dependents"] <- "3+"
  }
}


# FILLING MISSING VALUE OF LOAN AMOUNT
  ggplot(aes(x = ApplicantIncome, y = LoanAmount), 
         data = na.omit(combinedData))+
    geom_point()+
    xlim(0,9000)

# CROSS VALIDATION ON THE TOAL INCOME
  library(caret)
  
  set.seed(100)
  
  train_control <- trainControl(method = "cv", number = 10, repeats = 10)  
  
  fit.amount <- train(LoanAmount~TotalIncome, 
                      data = na.omit(subset(combinedData, TotalIncome <= 10000)),
                      method = "lm")
  print(fit.amount)

 
# FITTING THE MODEL TO PREDICT LOAN AMOUNT
  
  lm.fit <- lm(LoanAmount~TotalIncome,
               data = na.omit(subset(combinedData, TotalIncome <= 10000)))
  
  lm.predict <- predict(lm.fit, newdata = combinedData[which(is.na(LoanAmount)),])
  
  combinedData[which(is.na(LoanAmount)),"LoanAmount"] <- lm.predict
  
# FILLING SELF-EMPLOYED

  #Self Employed have more mean salary
  ggplot(aes(x = Self_Employed, y = ApplicantIncome),
         data = subset(combinedData, ApplicantIncome < 9000))+
    geom_bar(stat = "summary", fun.y = mean)+
    ggtitle("Self_Employed Vs Mean Applicant Income")
  
  ggplot(aes(x = Self_Employed, y = ApplicantIncome, fill = Property_Area),
         data = subset(subset(combinedData, !is.na(Self_Employed))),
         ApplicantIncome < 9000)+
    geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
    ggtitle("Self_Employed Vs Mean Applicant Income")
  
  ggplot(aes(x = Property_Area, y = ApplicantIncome), 
         data = na.omit(combinedData))+
  geom_bar(stat = "summary", fun.y = median, position = "dodge")
  
  bySelfEmp <- subset(combinedData, !is.na(combinedData$Self_Employed)) %>%
    filter(ApplicantIncome <= 9000)%>%
    group_by(Self_Employed, Property_Area) %>%
    summarise(mean = mean(ApplicantIncome),
              user = n())
  bySelfEmp
  
  # 10 FOLD CROSS VALIDATION ON SELF EMPLOYED
  
  incomeUpto9k <- combinedData[which(ApplicantIncome <= 9000),]
  set.seed(100)
  
  train_control <- trainControl(method = "cv", number = 10, repeats = 10)  
  
  fit.amount <- train(Self_Employed~ApplicantIncome, 
                      data = subset(incomeUpto9k, !is.na(incomeUpto9k$Self_Employed)),
                      method = "lda")
  print(fit.amount)
  
  # Training the model
  lda.fit <- lda(Self_Employed~ApplicantIncome, 
                 data = subset(incomeUpto9k, !is.na(incomeUpto9k$Self_Employed)))
  
  lda.predict <- predict(lda.fit, newdata= combinedData[which(is.na(Self_Employed)),])
  
# FILLING THE VALUES
  combinedData[which(is.na(Self_Employed)),"Self_Employed"] <- lda.predict$class 
  
  
 # FILLING LOAN TERM MISSING VALUES
  
  
  # LOAN AMOUNT TERM
    
  combinedData$EMI <- combinedData$LoanAmount*1000/as.numeric(levels(
    combinedData$Loan_Amount_Term))[combinedData$Loan_Amount_Term]
  
  ggplot(aes(x = Loan_Amount_Term, y = LoanAmount), data = 
           subset(combinedData, !is.na(Loan_Amount_Term)))+
    geom_boxplot()
  
  ggplot(aes(x = Loan_Amount_Term, y = TotalIncome), data = 
           subset(combinedData, !is.na(combinedData$Loan_Amount_Term)))+
    geom_boxplot()+
    coord_cartesian(ylim = c(0,10000))
  
  # Outlier In EMI above 800
  ggplot(aes(x = 0, y = EMI), data = combinedData)+
    geom_boxplot()+
    coord_cartesian(ylim = c(0,800))
  

  
  library(MASS)
  tempData  <- (combinedData[which(combinedData$TotalIncome <= 12500 &
                                     combinedData$EMI <= 800),])
  tempData$Loan_Amount_Term <- as.factor(tempData$Loan_Amount_Term)
  tempData$Loan_Amount_Term <- droplevels(tempData$Loan_Amount_Term)
  
  set.seed(12)
  cv <- train(Loan_Amount_Term~TotalIncome+EMI,
              data = subset(tempData, !is.na(tempData$Loan_Amount_Term)),
              method = "lda",
              trControl = trainControl(method = "cv", number = 10, repeats = 10))
  print(cv)
  
  
  loanTerm.fit <- lda(Loan_Amount_Term ~ TotalIncome, 
                  data = subset(tempData, !is.na(tempData$Loan_Amount_Term)))  
  
  loanTerm.predict <- predict(loanTerm.fit, newdata = subset(tempData, !is.na(tempData$Loan_Amount_Term)))
  
  table(loanTerm.predict$class, subset(tempData, !is.na(tempData$Loan_Amount_Term))$Loan_Amount_Term)

  mean(loanTerm.predict$class != subset(tempData, !is.na(tempData$Loan_Amount_Term))$Loan_Amount_Term)
  
#Predicting Missing NA Loan Term Values
  loanTermNa.predict <- predict(loanTerm.fit, newdata = combinedData[which(is.na(combinedData$Loan_Amount_Term)),])
  
  table(loanTermNa.predict$class)
  
  combinedData[which(is.na(combinedData$Loan_Amount_Term)),"Loan_Amount_Term"] <- loanTermNa.predict$class
 


# FIlling Missing Value of Credit History
  
 # Creating EMI Column Again
combinedData$EMI <- combinedData$LoanAmount*1000/as.numeric(levels(
  combinedData$Loan_Amount_Term))[combinedData$Loan_Amount_Term]

# Credit History Vs TotalIncome  Vs Proper_Area
  ggplot(aes(x = Credit_History, y = TotalIncome, fill = Property_Area),
         data = subset(combinedData, !is.na(Credit_History)))+
    geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
  ylim(c(0,11000))# Outlier greater than 12200 in Total Income
  
  ggplot(aes(x = Credit_History, fill = Self_Employed, y = TotalIncome),
         data = subset(combinedData, !is.na(Credit_History)))+
    geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
    facet_grid(~Property_Area)+
  ylim(c(0,11000))+
  ggtitle("Total Mean Salary Less Than 10k")  # Matters: SelfEmployed_Yes, Property
  
  # Applicant Mean Salary greater than 10k
  ggplot(aes(x = Credit_History, fill = Self_Employed, y = ApplicantIncome),
         data = na.omit(subset(loanData, loanData$ApplicantIncome >= 10000)))+
    geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
    facet_grid(~Property_Area)+
    ggtitle("Applicant Mean Salary Greater Than 10k")
  
  # Analysis On Total Income
  ggplot(aes(x = Credit_History, fill = Self_Employed, y = TotalIncome),
         data = na.omit(subset(loanData, loanData$TotalIncome <= 12200 & 
                                 loanData$ApplicantIncome <= 10000)))+
    geom_bar(stat = "summary", fun.y = mean, position = "dodge")+
    facet_grid(~Property_Area)+
    ggtitle("Total Mean Salary Lesser Than 12k")
  
  
  
  
  
 
  
  # Group by Applicant Income, Credit History, EMI
  loanData$EMI <- loanData$LoanAmount*1000/as.numeric(levels(
    loanData$Loan_Amount_Term))[loanData$Loan_Amount_Term]
  
  byLoanAmount <- na.omit(combinedData) %>%
    group_by(Credit_History,Property_Area,Self_Employed)%>%
    summarise(meanTotalIncome = mean(TotalIncome),
              meanEMI = mean(EMI),
              user = n())
  byLoanAmount
  
# Using Modelling Approach For Predicting Credit History
tempData <- subset(combinedData, combinedData$TotalIncome < 11000)
tempData <- subset(tempData, !is.na(tempData$Credit_History))
tempData$Loan_Status <- NULL

credit.cv <- train(Credit_History~.-Loan_ID-Loan_Amount_Term, 
                    data = subset(tempData, tempData$EMI < 800),
                    method = "lda",
                    trControl = trainControl(method = "cv", number = 10, repeats = 10))

credit.fit <- lda(Credit_History~.-Loan_ID-Loan_Amount_Term-ApplicantIncome-CoapplicantIncome-Gender-Married, 
                  data = subset(tempData, tempData$EMI < 800))

credit.predict <- predict(credit.fit,  tempData[which(is.na(tempData$Credit_History)),])
table(credit.predict$class)
mean(credit.predict$class != subset(tempData, tempData$EMI < 800)$Credit_History)


  #  Chossing the subset of Predictors
  ggplot(aes(x = Gender, fill = Loan_Status), data = 
           subset(loanData, !is.na(loanData$Credit_History)))+
    geom_bar()+
    facet_wrap(~Credit_History)
  
  ggplot(aes(x = Dependents, fill = Loan_Status), data = loanData)+
    geom_bar()+
    facet_wrap(~Married)
  
  ggplot(aes(x = Education, fill = Loan_Status), data = 
           subset(loanData, !is.na(loanData$Credit_History)))+
    geom_bar()+
    facet_wrap(~Credit_History)
  
  ggplot(aes(x = Self_Employed, fill = Loan_Status), data = 
           subset(loanData, !is.na(loanData$Credit_History)))+
    geom_bar()+
    facet_wrap(~Credit_History) # People who are self-employed with credit 0 didnt get loan
  
  ggplot(aes(x = Loan_Status, y = ApplicantIncome), data = incomeUpto10k)+
    geom_bar(stat = "summary", fun.y = mean)+
    facet_wrap(~Self_Employed+Education)
  
  ggplot(aes(x = Loan_Status, y = TotalIncome), data = loanData)+
    geom_boxplot()+
    facet_wrap(~Self_Employed)+
    ylim(c(0,8000))
  
  # Using Lasso For Feature Selection
  set.seed(12)
  train <- sample(1:559, 450)
  loanData2 <- na.omit(loanData)
  test.X = model.matrix(Loan_Status ~., data = loanData2[train,-1])[,-1]
  train.Y = loanData2$Loan_Status[train]
  test.Y <- model.matrix(Loan_Status ~., data = loanData2[-train,-1])[,-1]
  grid <- 10^seq(10,-2 , length = 100)
  
  library(glmnet)
  lasso.mod <- cv.glmnet(test.X, train.Y, alpha = 1, family = "binomial", nlambda = 100,
                         type.measure = "class")
  plot(lasso.mod)
  
  lasso.fit <- glmnet(test.X, train.Y, alpha = 1, family = "binomial", lambda = lasso.mod$lambda.min)
  lasso.fit$beta[,1]
  # Perform Cross Validation
  
  library(caret)
  set.seed(12)
  tarin_control1 <- trainControl(method = "cv", number = 10, repeats = 10)
  kfoldcv <- train(Loan_Status ~ Credit_History, data = loanData2, method = "lda" , trControl = train_control)
  print(kfoldcv)
  
  tarin_control2 <- trainControl(method = "cv", number = 10, repeats = 10)
  kfoldcv <- train(Loan_Status ~ Credit_History+ApplicantIncome, data = loanData2, method = "lda" , trControl = train_control)
  print(kfoldcv)
  
  # TRaining the model
  lda.fit <- lda(Loan_Status ~ Credit_History, data = loanData2)
  
  
 
  
  loanStatus.predict <- predict(lda.fit, newdata = na.omit(loanDataTestfile))
  
  summary(loanDataTestfile$Self_Employed)

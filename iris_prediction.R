# Collecting and Preparing Data
library(MASS)
data("iris")
str(iris)

# Exploring data 
summary(iris)
head(iris)
tail(iris)
library(GGally)
ggpairs(iris,  mapping=ggplot2::aes(color = Species))

library(ggplot2)
ggplot(aes(x = Sepal.Length, y = Petal.Length), data = iris)+
  geom_point(aes(color = Species))+
  geom_smooth(method = "loess", aes(color = Species))


# Choosing best model using best subset regression
library(leaps)
subset.reg <- regsubsets(Species~., data = iris)
summary(subset.reg)$adjr2
summary(subset.reg)$cp
summary(subset.reg)$bic


# Chosing best model using backward approach
backward.reg <- regsubsets(Species~., data = iris, method = "backward")
summary(backward.reg)
summary(backward.reg)$adjr2
summary(backward.reg)$cp
summary(backward.reg)$bic


# Chosing best model using forward approach
forward.reg <- regsubsets(Species~., data = iris, method = "forward")
summary(forward.reg)
summary(forward.reg)$adjr2
summary(forward.reg)$cp
summary(forward.reg)$bic


# Using 10 fold CROSS VALIDATION 

library(caret)
train_control <- trainControl(method = "cv", number = 10, repeats = 10)
model <- train(Species~., data = iris, trControl = train_control,
               method = "lda")
print(model)


# Creating the model

library(MASS)
lda.fit <- lda(Species ~., data = iris)

# Prediction on the test data
lda.predict <- predict(lda.fit, newdata = iris)

# Confusion Matrix
table(lda.predict$class, iris$Species)

# Mean Percentage Error in Model Prediction
mean(lda.predict$class != iris$Species)*100

# Using Trees For Prediction

library(tree)
iris.tree <- tree(Species~., data = iris)
summary(iris.tree)
plot(iris.tree)
text(iris.tree, pretty = 0)
iris.tree

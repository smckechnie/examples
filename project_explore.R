library(readr)
library(dplyr)
library(glmnet)

# Import data
setwd("C:/Users/steph/OneDrive/Documents/Stat 281/Project")
setwd("C:/Users/mckec/OneDrive/Documents/Stat 281/Project")

#train_main <- read.csv("smartphone_train.csv",header = T) # Slow load
train_main <- read_csv('smartphone_train.csv')
test <- read_csv('smartphone_test.csv')

train <- as.data.frame(train_main[,c(1:6,563)])
colnames(train) <- c("X1","X2","X3","X4","X5","X6","Activity")

train_main_labels <- train_main$Activity
test_labels <- test$Activity


library(ggplot2)
ggplot(train, aes(x = X1, y = X2, colour = Activity)) + geom_point()
ggplot(train, aes(x = X1, y = X3, colour = Activity)) + geom_point()
ggplot(train, aes(x = X1, y = X4, colour = Activity)) + geom_point()
ggplot(train, aes(x = X1, y = X5, colour = Activity)) + geom_point()
ggplot(train, aes(x = X1, y = X6, colour = Activity)) + geom_point()

ggplot(train, aes(x = X4, y = X5, colour = Activity)) + geom_point()
ggplot(train, aes(x = X4, y = X6, colour = Activity)) + geom_point()

plot(train[,1],train[,2])

# Fix duplicate colnames
names(train_main) <- make.names(names(train_main), unique = TRUE)
names(test) <- names(train_main)

# Remove labels from training and test sets
train_main <- subset(train_main,select=-c(Activity))
test <- subset(test,select=-c(Activity))

# Create a calibration and training set from the main training set
set.seed(42)
idx = sample(c(TRUE, FALSE), nrow(train_main), replace = TRUE, prob = c(0.8, 0.2))

train = subset(train_main, idx)
train_labels  = subset(train_main_labels, idx)

cal = subset(train_main, !idx)
cal_labels = subset(train_main_labels, !idx)

x <- model.matrix(train_main_labels ~ ., train_main)[,-1]
fit = glmnet(x, train_main_labels, family = "multinomial", type.multinomial = "grouped")

plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
plot(fit, xvar = "dev", label = TRUE)

cvfit=cv.glmnet(x, train_main_labels, family="multinomial", type.measure="class", 
                type.multinomial = "grouped", nfolds = 5, parallel = TRUE)
plot(cvfit)

# Best lambda 
print(cvfit$lambda.1se)
print(cvfit$lambda.min)
bestlam = cvfit$lambda.1se
tail(cvfit$lambda)

nx = model.matrix(cal_labels ~., cal)[,-1]
cal.pred <- predict(cvfit, s = bestlam, newx = nx, type = "class")

table(cal.pred, cal_labels)
acc = sum(cal.pred == cal_labels) / length(cal.pred)
cat(paste0("Our accuracy against our calibration dataset is: ", acc))

testx = model.matrix(test_labels ~., test)[,-1]
test.pred <- predict(cvfit, s = bestlam, newx = testx, type = "class")
acc = sum(test.pred == test_labels) / length(test.pred)
cat(paste0("Our accuracy against our test dataset is: ", acc))

library(caret)
confusionMatrix(test.pred, test_labels)

library(class)
out <- knn(train_main, test, train_main_labels, k = 3, prob=TRUE)
comp.out <- cbind(out,as.factor(test_labels))
sum(comp.out[,1]==comp.out[,2])/dim(comp.out)[1] #0.808


library(randomForest)
train_levels <- as.numeric(as.factor(train_main_labels))

fit <- randomForest(x=train_main,y=train_levels, xtest=test, importance=TRUE,ntree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
as.numeric(levels(as.factor(train_main_labels)))[train_main_labels]

#PCA
train.pca <- prcomp(train_main,center = TRUE,scale. = TRUE)
print(train.pca)
zapsmall(train.pca$rotation[,1])
train.pca$rotation[1:5,1:4]
dim(train.pca$x)
summary(train.pca)

plot(train.pca, type = "l")
biplot(train.pca,scale = 0)


std_dev <- train.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


ind <- order(abs(train.pca$rotation[,1]),decreasing = T)
zapsmall(train.pca$rotation[ind,1])

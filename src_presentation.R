setwd("C:/Users/steph/OneDrive/Documents/Stat 281/Project")
setwd("C:/Users/mckec/OneDrive/Documents/Stat 281/Project")
# Import data
library(readr)

train_main <- read_csv('smartphone_train.csv')
test <- read_csv('smartphone_test.csv')

train_labels <- train_main$Activity
train_levels <- as.numeric(as.factor(train_labels))

test_labels <- test$Activity

# Remove labels from training and test sets
train_main <- subset(train_main,select=-c(Activity,subject))
test <- subset(test,select=-c(Activity,subject))

var.names <- make.names(names(train_main), unique = TRUE)
names(train_main) <- 1:dim(train_main)[2]
names(test) <- 1:dim(test)[2]


library(rpart)
library(rpart.plot)

fit <- rpart(train_labels~., data=train_main,method="class")
fit

rpart.plot(fit)
rpart.plot(fit,under=TRUE)
rpart.plot(fit,fallen.leaves=FALSE)
rpart.plot(fit,varlen=10)
rpart.plot(fit,tweak=1.5)

pdf("tree.pdf",width=10.5, height=10)
rpart.plot(fit,varlen=10,tweak=1.2)
dev.off()

pred <- predict(fit,test,type="class")
accuracy <- table(pred,test_labels)
accuracy
sum(diag(accuracy))/sum(accuracy)

temp <- as.data.frame.matrix(accuracy)
stargazer(as.matrix(temp))


require(randomForest)
library(caret)

names(train_main) <- var.names
names(test) <- var.names

fit.class <- randomForest(as.factor(train_labels)~., data=train_main, importance=T)
fit.class

pred2 <- predict(fit.class, newdata = test)
table(pred2, test_labels)
accur <- table(pred2, test_labels)
sum(diag(accur))/sum(accur)

temp <- as.data.frame.matrix(accur)
stargazer(as.matrix(temp))

library(class)
pred.knn <- knn(train_main, test, cl = factor(train_labels), k = 6)

table(pred.knn, test_labels)
accur <- table(pred.knn, test_labels)
sum(diag(accur))/sum(accur)

rowsums = apply(accur, 1, sum)
colsums = apply(accur, 2, sum)

precision = diag(accur) / colsums 
recall = diag(accur) / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 


#### additional methods
# Support vector machine
library(e1071)

model <- svm(as.factor(train_labels)~., data=train_main,type="C-classification")
# perform a grid search
tuneResult <- tune.svm(as.factor(train_labels)~., data=train_main, gamma = 10^(-6:-1), cost = c(1,10))
summary(tuneResult)
# plot(tuneResult)

pred3 <- predict(model, newdata = test)
table(pred3, test_labels)
accur <- table(pred3, test_labels)
sum(diag(accur))/sum(accur)

# Neural Network
library(neuralnet)

f <- as.formula(paste("activity ~", paste(var.names[!var.names %in% "activity"], collapse = " + ")))
train_main$activity <- as.factor(train_labels)
train_main$activity <- train_levels
nn <- neuralnet(f,data=train_main,hidden=6, lifesign = "minimal", linear.output=FALSE)
plot(nn, rep = "best")

pred4 <- predict(nn, newdata = test)
table(pred3, test_labels)
accur <- table(pred3, test_labels)
sum(diag(accur))/sum(accur)

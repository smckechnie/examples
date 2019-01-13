library(readr)
#library(dplyr)
#library(glmnet)

# Import data
setwd("C:/Users/steph/OneDrive/Documents/Stat 281/Project")
setwd("C:/Users/mckec/OneDrive/Documents/Stat 281/Project")

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

require(randomForest)
library(caret)
names(train_main) <- var.names

fit <- randomForest(train_levels~., data=train_main, importance=T)
fit$importance
var.imp <- fit$importance
zapsmall(var.imp[order(var.imp[,1],decreasing = T),])
zapsmall(var.imp[order(var.imp[,2],decreasing = T),])

ind.sort <- order(var.imp[,2],decreasing = T)
var.imp.sort <- var.imp[ind.sort ,]
zapsmall(var.imp.sort[1:64,])

plot(fit,log="y")
varImp(fit)
varImpPlot(fit)


fit.class <- randomForest(as.factor(train_labels)~., data=train_main, importance=T)
fit.class
var.imp.cl <- fit.class$importance
zapsmall(var.imp.cl[order(var.imp.cl[,7],decreasing = T),])
zapsmall(var.imp.cl[order(var.imp.cl[,8],decreasing = T),])

zapsmall(var.imp.cl[order(var.imp.cl[,1],decreasing = T),])
zapsmall(var.imp.cl[order(var.imp.cl[,2],decreasing = T),])
zapsmall(var.imp.cl[order(var.imp.cl[,4],decreasing = T),])

names(test) <- var.names

pred <- predict(fit.class, newdata = test)
table(pred, test_labels)
accur <- table(pred, test_labels)
sum(diag(accur))/sum(accur)


fit.rf <- randomForest(train_levels~., data=train_main, proximity=TRUE, keep.forest=FALSE)
tree <- getTree(fit,1,labelVar=TRUE)
d <- to.dendrogram(tree)
str(d)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))


pdf("plot1.pdf")
ggplot(train_main, aes(x = tGravityAcc.arCoeff...Y.2, y = tBodyAccMag.std.., colour = train_labels)) + geom_point() + theme(legend.position="top")
dev.off()

pdf("plot2.pdf")
ggplot(train_main, aes(x = angle.Y.gravityMean., y = fBodyAccJerk.mad...X, colour = train_labels)) + geom_point() + theme(legend.position="top")
dev.off()

pdf("plot3.pdf")
ggplot(train_main, aes(x = tGravityAcc.mean...X , y = angle.Y.gravityMean., colour = train_labels)) + geom_point() + theme(legend.position="top")
dev.off()

ggplot(train_main, aes(x = tGravityAcc.mean...X , y = fBodyAcc.mean...X, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tGravityAcc.mean...X , y = angle.Y.gravityMean., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = fBodyAcc.mean...X , y = angle.Y.gravityMean., colour = train_labels)) + geom_point()

ggplot(train_main, aes(x = fBodyAcc.mean...X , y = fBodyAccMag.energy.., colour = train_labels)) + geom_point()



ggplot(train_main, aes(x = tGravityAcc.mean...X , y = tBodyGyroJerk.iqr...Z, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyAcc.correlation...X.Y, y = tBodyAccJerkMag.entropy.., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyGyro.correlation...Y.Z, y = tBodyAcc.correlation...X.Y, colour = train_labels)) + geom_point()

ggplot(train_main, aes(x = tGravityAcc.mean...X , y = angle.Y.gravityMean., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyAcc.entropy...X , y = angle.Y.gravityMean., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyAcc.entropy...X , y = tGravityAcc.mean...X , colour = train_labels)) + geom_point()

ggplot(train_main, aes(x = angle.X.gravityMean., y = angle.Y.gravityMean., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = angle.X.gravityMean., y = tGravityAcc.mean...X , colour = train_labels)) + geom_point()



ggplot(train_main, aes(x = fBodyAccJerk.bandsEnergy...1.16, y = fBodyAccJerk.bandsEnergy...1.8, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = fBodyAccJerk.bandsEnergy...1.16, y = tBodyAccJerk.mad...X, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = fBodyAccJerk.max...X, y = tBodyAccJerk.mad...X, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tGravityAcc.mean...X , y = tBodyAccJerk.mad...X, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tGravityAcc.mean...X , y = tBodyGyroJerk.iqr...Z, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyAcc.correlation...X.Y, y = tBodyAccJerkMag.entropy.., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tGravityAcc.arCoeff...Y.2, y = tBodyAccJerkMag.entropy.., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tGravityAcc.arCoeff...Y.2, y = tBodyAccMag.std.., colour = train_labels)) + geom_point()

ggplot(train_main, aes(x = angle.Y.gravityMean., y = fBodyAccJerk.mad...X, colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = angle.Y.gravityMean., y = angle.Z.gravityMean., colour = train_labels)) + geom_point()
ggplot(train_main, aes(x = tBodyGyroJerk.mad...Z, y = tBodyGyroJerk.iqr...Z , colour = train_labels)) + geom_point()


to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}












train.pca <- prcomp(train_main,center = TRUE,scale. = TRUE)
print(train.pca)
zapsmall(train.pca$rotation[,1])
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
ind[1:60]

require(randomForest)
library(caret)
p <- 100

temp <- train_main[,ind[1:p]]
colnames(temp) <- var.names[ind[1:p]]
temp <- cbind(temp,train_main_labels)

fit <- randomForest(train_main_labels~., data=as.data.frame(temp))
varImp(fit)

var.imp <- varImp(fit)
var.imp[order(var.imp,decreasing = T),]

library(ggplot2)
ggplot(temp, aes(x = fBodyGyro.mean...X, y = tBodyAcc.std...X, colour = train_main_labels)) + geom_point()
ggplot(temp, aes(x = fBodyGyro.mean...X, y = tBodyAcc.max...X, colour = train_main_labels)) + geom_point()
ggplot(temp, aes(x = tBodyAcc.max...X, y = tBodyAcc.std...X, colour = train_main_labels)) + geom_point()

ggplot(temp, aes(x = fBodyAcc.sma.., y = fBodyAccJerk.sma.., colour = train_main_labels)) + geom_point()
ggplot(temp, aes(x = fBodyAcc.sma.., y = tBodyAccJerk.sma.., colour = train_main_labels)) + geom_point()
ggplot(temp, aes(x = fBodyAccJerk.sma.., y = tBodyAccJerk.sma.., colour = train_main_labels)) + geom_point()

ggplot(temp, aes(x = fBodyAcc.sma.., y = tBodyAcc.min...X, colour = train_main_labels)) + geom_point()
ggplot(temp, aes(x = fBodyAcc.sma.., y = tBodyAccJerk.std...X, colour = train_main_labels)) + geom_point()


library("FactoMineR")
res.pca <- PCA(train_main, graph = FALSE)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(res.pca, ncp=10)

head(res.pca$var$coord)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

head(res.pca$var$contrib)
fviz_pca_contrib(res.pca, choice = "var", axes = 1)

fviz_contrib(res.pca, choice = "var", axes = 1)
fviz_contrib(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 3)
fviz_contrib(res.pca, choice = "var", axes = 4)
fviz_contrib(res.pca, choice = "var", axes = 5)

fviz_contrib(res.pca, choice = "var", axes = 1:2)
fviz_contrib(res.pca, choice = "var", axes = 1:5)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 25)

fviz_pca_var(res.pca, col.var="contrib")

res.desc <- dimdesc(res.pca, axes = c(1,2))
res.desc$Dim.1
res.desc$Dim.2

res.desc$Dim.1$quanti[500:545,]

fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()



# Dimension Reduce
pc.train <- princomp(train_main)
pc.train <- prcomp(train_main, center = TRUE, scale. = TRUE)

summary(pc.train)
loadingX <- loadings(pc.train)
zapsmall(loadingX[,1:2]) 
plot(pc.train)
biplot(pc.train)

scores_df <- as.data.frame(pc.train$x)
head(scores_df[1:2])
plot(PC1~PC2, data=scores_df, cex = .1, lty = "solid")
library(scales)
ramp <- colorRamp(c("yellow", "blue"))
colours_by_mean <- rgb( 
  ramp( as.vector(rescale(rowMeans(train_main),c(0,1)))), 
  max = 255 )
plot(PC1~PC2, data=scores_df,cex = .1, lty = "solid", col=colours_by_mean)


# by Random forest
require(randomForest)
library(caret)

var.names <- make.names(names(train_main), unique = TRUE)
names(train_main) <- 1:562
names(train_main) <- var.names

fit <- randomForest(train_levels~., data=train_main, importance=T)
fit$importance
var.imp <- fit$importance
var.imp[order(var.imp[,1],decreasing = T),]
zapsmall(var.imp[order(var.imp[,1],decreasing = T),])

varImp(fit)
varImpPlot(fit)

var.imp <- varImp(fit)
var.imp[order(var.imp,decreasing = T),]




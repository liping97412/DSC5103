##########################################################
# DSC5103 Statistics
# Session 8. Trees II: Trees, Bagging and Random Forests
# 2018.10
#
# Fitting Regression Trees, Bagging, Random Forest, and Gradient Boosting Machine (later)
# demo for Regression problems
# using the Boston housing dataset in package "MASS"
##########################################################


### the Boston housing data
library("MASS")
?Boston
summary(Boston)
Boston$chas <- as.factor(Boston$chas)

# separate training and test
set.seed(2345)
train.index <- sample(1:nrow(Boston), nrow(Boston)/2)
test.index <- -train.index
x.test <- Boston[test.index, -14]
y.test <- Boston[test.index, "medv"]



### Tree as a benchmark
library("tree")
# grow a tree
boston.tree <- tree(medv ~ ., Boston, subset=train.index)
summary(boston.tree)
plot(boston.tree)
text(boston.tree)
# CV
set.seed(12345)
boston.tree.cv <- cv.tree(boston.tree)
plot(boston.tree.cv, type="b")
# prune
boston.tree.pruned <- prune.tree(boston.tree, best=11)
plot(boston.tree.pruned)
text(boston.tree.pruned)
# predict
yhat.tree <- predict(boston.tree.pruned, newdata=x.test)
plot(yhat.tree, y.test)
abline(0,1)
# MSE
mse.tree <- mean((yhat.tree - y.test)^2)



### Bagging
library("randomForest")
set.seed(12)  # there is randomness in the bootstrap part of bagging
# fit a bagging model (randomForest when mtry == p)
boston.bag <- randomForest(medv ~ ., data=Boston, subset=train.index, mtry=13)
boston.bag
plot(boston.bag)
# predict
yhat.bag <- predict(boston.bag, newdata=x.test)
plot(yhat.bag, y.test)
abline(0,1)
mse.bag <- mean((yhat.bag - y.test)^2)



### Random Forests
# fit a random forest model
set.seed(12)  # RF is random in both the bootstrap part and the predictor selection part
boston.rf <- randomForest(medv ~ ., data=Boston, subset=train.index)  # default mtry=p/3 for regression
boston.rf
plot(boston.rf)
# predict
yhat.rf <- predict(boston.rf, newdata=x.test)
plot(yhat.rf, y.test)
abline(0,1)
mse.rf <- mean((yhat.rf - y.test)^2)

# variable importance
importance(boston.rf)  # by default, we only have IncNodePurity
varImpPlot(boston.rf)
# rebuild the model with "importance=TRUE" for more variable importance (%IncMSE)
boston.rf3 <- randomForest(medv ~ ., data=Boston, subset=train.index, importance=TRUE)
boston.rf3
importance(boston.rf3)
varImpPlot(boston.rf3)

# partial plot in RF
?partialPlot
partialPlot(boston.rf, Boston[train.index, ], x.var="rm")
partialPlot(boston.rf, Boston[train.index, ], x.var="rad")
partialPlot(boston.rf, Boston[train.index, ], x.var="chas")


## tuning RF
# tune random forest (mtry) by tuneRF (highly variable)
?tuneRF
set.seed(12)
tuneRF(x=Boston[train.index, -14], y=Boston[train.index, 14], mtryStart=4, ntreeTry=500, stepFactor=1.25)

# tune random forest (mtry) manually
mse.rfs <- rep(0, 13)
for(m in 1:13){
    #set.seed(12)
    rf <- randomForest(medv ~ ., data=Boston, subset=train.index, mtry=m)
    mse.rfs[m] <- rf$mse[500]
}
plot(1:13, mse.rfs, type="b", xlab="mtry", ylab="OOB Error")
mse.rfs


## predict on test data directly
set.seed(12)
boston.bag2 <- randomForest(medv ~ ., data=Boston, subset=train.index, mtry=13, xtest=x.test, ytest=y.test)
boston.bag2
set.seed(12)
boston.rf2 <- randomForest(medv ~ ., data=Boston, subset=train.index, mtry=7, xtest=x.test, ytest=y.test)
boston.rf2
str(boston.rf2)
boston.rf2$test
mse.bag2 <- boston.bag2$test$mse[500]
mse.rf2 <- boston.rf2$test$mse[500]

# plot OOB error and test error together
plot(boston.rf2$mse, type="l", col="blue", ylim=c(10, 30), xlab="Number of Trees", ylab="MSE")
lines(boston.rf2$test$mse, lwd=3, col="blue")
lines(boston.bag2$mse, col="red")
lines(boston.bag2$test$mse, lwd=3, col="red")
abline(h=mse.tree, lty=2)
legend(350, 25, c("Bagging - OOB error", "Bagging - Test error", "RF - OOB error", "RF - Test error"), col=c("red", "red", "blue", "blue"), lty=c(1, 1, 1, 1), lwd=c(1, 3, 1, 3))




##########################################################
# DSC5103 Statistics
# Session 5. Demo of Cross-Validation of Logistic Regression classification on the mixture.example dataset
# 2017.9
#
# -- based on the mixture.example dataset and documentation in the "ElemStatLearn" package
##########################################################


#############################
### Loading data
#############################
# load the dataset "mixture.example" in package "ElemStatLearn")
library("ElemStatLearn")  # run install.packages("ElemStatLearn") if you haven't



#############################
### Exloration
#############################

# copy important ones out
x <- mixture.example$x
y <- mixture.example$y
prob <- mixture.example$prob
xnew <- mixture.example$xnew
px1 <- mixture.example$px1
px2 <- mixture.example$px2


# make dataframe for x and y (for ggplot use)
df.training <- data.frame(x1=x[ , 1], x2=x[ , 2], y=y)
df.training$y <- as.factor(df.training$y)

# dataframe for plotting the boundary
df.grid <- data.frame(x1=xnew[ , 1], x2=xnew[ , 2])
df.grid$prob <- prob
summary(df.grid)


# plot X and Y
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()
p0

# add the true boundary into the plot
p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))
p.true



#############################
### Cross-Validation
#############################
K <- 10  # 10-fold CV
RUN <- 20  # the number of repetitions of CV
I <- 9  # the max polynomical order to consider


#############################
## auto CV with cv.glm()
#############################
library("boot")
# prepare an empty data.frame to save the MSE for each *run* and *i*
err.kfold <- expand.grid(run=factor(1:RUN), i=1:I)
err.kfold$err <- 0

# "cost" is set to be the misclassification error with cutoff=0.5
cost <- function(y, p = 0) {mean(y != (p > 0.5))}

set.seed(1688)
for (run in 1:RUN) {
    for (i in 1:I) {
        glm.fitted <- glm(y ~ poly(x1, i, raw=TRUE) + poly(x2, i, raw=TRUE), family=binomial(), data=df.training)
        err.kfold[err.kfold$run == run & err.kfold$i == i, "err"] <- cv.glm(df.training, glm.fitted, cost, K=K)$delta[1]  # specify K=10 for k-fold CV
    }
}
head(err.kfold)
# plot the Error for each run and each i
ggplot(data=err.kfold) + geom_line(aes(x=i, y=err, color=run)) + theme_bw()



#############################
## manual k-fold Cross-Validation
#############################

# test randomly partition data into K chunks
N <- nrow(df.training)
# method 1: unbalanced folds (as in cv.tree())
fold <- sample(1:K, N, replace=TRUE)
table(fold)

# method 2: a more balanced way (as in cv.glm() and cv.glmnet())
fold <- sample(rep(seq(K), length=N))
table(fold)


## manual k-fold Cross-Validation for misclassification rate
err.kfold2 <- expand.grid(run=factor(1:RUN), i=1:I)
err.kfold2$err <- 0

set.seed(1688)
for (run in 1:RUN) {
    # create a random partition
    fold <- sample(rep(seq(K), length=N))
    for (i in 1:I) {
        err <- 0  # overall misclassfication errors in all k folds
        # start the k-fold CV
        for (k in 1:K) {
            glm.fitted <- glm(y ~ poly(x1, i, raw=TRUE) + poly(x2, i, raw=TRUE), family=binomial(), data=df.training[fold != k, ])
            glm.prob <- predict(glm.fitted, newdata=df.training[fold == k, ], type="response")
            glm.pred <- ifelse(glm.prob > 0.5, 1, 0)
            glm.err <- sum(glm.pred != df.training[fold == k, "y"])
            err <- err + glm.err
        }
        err.kfold2[err.kfold2$run == run & err.kfold2$i == i, "err"] <- err / nrow(df.training)
    }
}
head(err.kfold2)
# plot the Error for each run and each i
ggplot(data=err.kfold2) + geom_line(aes(x=i, y=err, color=run)) + theme_bw()



## manual k-fold Cross-Validation for ROC plots and AUC
library("ROCR")
# here we just illustrate one model with i=7
i <- 7
auc <- rep(0, RUN)
set.seed(16888)
for (run in 1:RUN) {
    # create a random partition
    fold <- sample(rep(seq(K), length=N))
    # start the k-fold CV
    for (k in 1:K) {
        glm.fitted <- glm(y ~ poly(x1, i, raw=TRUE) + poly(x2, i, raw=TRUE), family=binomial(), data=df.training[fold != k, ])
        df.training[fold == k, "prob"] <- predict(glm.fitted, newdata=df.training[fold == k, ], type="response")
    }
    pred <- prediction(df.training$prob, df.training$y)
    perf <- performance(pred, measure="tpr", x.measure="fpr")
    plot(perf, add=(run != 1))
    auc[run] <- as.numeric(performance(pred, "auc")@y.values)
}
abline(a=0, b=1, lty=2)

# AUC in each run
plot(1:RUN, auc, type="l")
mean(auc)

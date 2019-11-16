##########################################################
# DSC5103 Statistics
# Session 5. Demo of Cross-Validation on GLM
# 2017.9
#
# using the Auto dataset in package "ISLR"
# CV for glm(mpg ~ poly(horsepower, i), data=Auto)
##########################################################


library("ggplot2")
library("ISLR")
summary(Auto)
N.row <- nrow(Auto)

#############################
### The Validation Set Approach (50-50)
#############################

RUN <- 10  # the number of repetitions
I <- 10  # the max polynomical order to consider

# prepare an empty data.frame to save the MSE for each *i* and *run*
mse.validation <- expand.grid(i=1:I, run=factor(1:RUN))
mse.validation$mse <- 0
head(mse.validation)

# start fitting models
set.seed(1)
for (run in 1:RUN) {
    # in each run, first generate an index indicating which rows in data will be used as training data
    train.index <- sample(N.row, round(0.5 * N.row))  # sample 50%
    validation.index <- - train.index
    # fit the i-th order polynomial regression
    for (i in 1:I) {
        glm.fitted <- glm(mpg ~ poly(horsepower, i, raw=TRUE), data=Auto, subset=train.index)  # use the training data
        pred <- predict(glm.fitted, newdata=Auto[validation.index, ])
        mse <- mean((Auto[validation.index, c("mpg")] - pred)^2)  # validation MSE calculation
        mse.validation[mse.validation$i == i & mse.validation$run == run, "mse"] <- mse  # save the MSE into the data.frame
    }
}
head(mse.validation)
summary(mse.validation)

# plot the MSE for each run and each i
ggplot(data=mse.validation) + geom_line(aes(x=i, y=mse, color=run)) + theme_bw()



#############################
### Leave-One-Out Cross-Validation
#############################
# for glm models, there exists a built-in function cv.glm() for doing LOOCV and k-fold CV
# test LOOCV using cv.glm()
library("boot")
?cv.glm
glm.fitted <- glm(mpg ~ horsepower, data=Auto)
cv.out <- cv.glm(Auto, glm.fitted)  # default is LOOCV
str(cv.out)
cv.out$delta[1]  # the CV Error is in delta[1]

# LOOCV
mse.LOOCV <- data.frame(i=1:I)
mse.LOOCV$mse <- 0
# fit the i-th order polynomial regression
for (i in 1:I){
    glm.fitted <- glm(mpg ~ poly(horsepower, i, raw=TRUE), data=Auto)
    mse.LOOCV[i, "mse"] <- cv.glm(Auto, glm.fitted)$delta[1]
}
mse.LOOCV

# plot the MSE for each i
ggplot(data=mse.LOOCV) + geom_line(aes(x=i, y=mse), color="black") + theme_bw()



#############################
### k-Fold Cross-Validation
#############################

# prepare an empty data.frame to save the MSE for each *i* and *run*
mse.kfold <- expand.grid(i=1:I, run=factor(1:RUN))
mse.kfold$mse <- 0

set.seed(168)
for (run in 1:RUN) {
    for (i in 1:I) {
        glm.fitted <- glm(mpg ~ poly(horsepower, i, raw=TRUE), data=Auto)
        mse.kfold[mse.kfold$i == i & mse.kfold$run == run, "mse"] <- cv.glm(Auto, glm.fitted, K=10)$delta[1]  # specify K=10 for k-fold CV
    }
}
head(mse.kfold)

# plot the MSE for each run and each i
ggplot(data=mse.kfold) + geom_line(aes(x=i, y=mse, color=run)) + theme_bw()



## Effect of k in k-fold CV
#############################

RUN2 <- 50  # number of repetitions
K <- 30  # maximum k to try
mse.k <- expand.grid(run=1:RUN2, k=2:K)
mse.k$mse <- 0
glm.fitted <- glm(mpg ~ poly(horsepower, 7, raw=TRUE), data=Auto)
set.seed(16789)
for (run in 1:RUN2) {
    for (k in 2:K) {
        mse.k[mse.k$k == k & mse.k$run == run, "mse"] <- cv.glm(Auto, glm.fitted, K=k)$delta[1]
    }
}
head(mse.k)

# plot the MSE for each run and each k
library(Hmisc)
ggplot(data=mse.k, aes(x=k, y=mse)) + geom_point(size=1) + theme_bw() +
stat_summary(fun.y=mean, geom="line", color="red") +  # add a line for the means
stat_summary(fun.data=mean_sdl, geom="errorbar", width=0.5, color="red")  # add errorbar with +/- one standard deviation

# or we can calculate the mean and standard deviation manuallyt
library("dplyr")
mse.k %>% group_by(k) %>% summarize(mean=mean(mse), sd=sd(mse))

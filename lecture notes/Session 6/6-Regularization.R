##########################################################
# DSC5103 Statistics
# Session 6. Regularization in Regression
# 2018.9
#
# run Ridge regression and LASSO using the glmnet() and cv.glmnet() function in the "glmnetUtils" package
# demo for regression only, classfication problems are in assignment 3
# using the Hitters dataset in package "ISLR"
##########################################################

## get the dataset ready
library("ISLR")

names(Hitters)
dim(Hitters)

# the Salary column has NA's
sum(is.na(Hitters$Salary))  # number of NA
# remove NA's in Salary
Hitters <- na.omit(Hitters)
sum(is.na(Hitters))  # number of NA
dim(Hitters)
summary(Hitters)




### Ridge Regression
# run Ridge Regression using glmnet() (alpha=0 means Ridge Regression)
library("glmnetUtils")

?glmnet
ridge.mod <- glmnet(Salary ~ ., Hitters, alpha=0, use.model.frame=TRUE)  # one can also specify a vector of lambdas to try by adding "lambda=c(1, 10, 100, 1000)"

# check the fitted model
str(ridge.mod)

# plot the fitted model
?plot.glmnet
plot(ridge.mod, xvar="lambda", label=TRUE)

# check the coefficient output
dim(coef(ridge.mod))  # 20 coefficients for the 19 variables, for each lambda
coef(ridge.mod)

# for example, the 50-th lambda
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
# the penalty term for the 50-th lambda
sum(coef(ridge.mod)[-1,50]^2)

# compare with the 60-th lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
# the penalty term for the 60-th lambda
sum(coef(ridge.mod)[-1,60]^2)

# can also specify a lambda (=1000) and "predict" the coefficients
coef(ridge.mod, s=1000)


## use CV to find optimal lambda
# CV
?cv.glmnet
set.seed(1)
ridge.cv <- cv.glmnet(Salary ~ ., Hitters, alpha=0, use.model.frame=TRUE)  # cv.glmnet() can set lambdas automatically
plot(ridge.cv)

# optimal lambda
str(ridge.cv)
ridge.lam <- ridge.cv$lambda.min
log(ridge.lam)
min(ridge.cv$cvm)
points(log(ridge.lam), min(ridge.cv$cvm), cex=3)

#### alternatively, set optimal lambda to lambda.1se for a more parsimonious model ####
ridge.lam2 <- ridge.cv$lambda.1se
log(ridge.lam2)
min(ridge.cv$cvm) + ridge.cv$cvsd[which.min(ridge.cv$cvm)]
points(log(ridge.lam2), min(ridge.cv$cvm) + ridge.cv$cvsd[which.min(ridge.cv$cvm)], cex=3)


# plot optimal lambda
plot(ridge.cv$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(ridge.lam), lty=2)
abline(v=log(ridge.lam2), lty=2)


## final model using optimal lambda
# predict(..., type="coefficient", s=lambda, ...) shows the model coefficients at lambda 
# we can predict use the cv.glmnet object
coef(ridge.cv, s=ridge.lam2)
# or use the glmnet object we had earlier
coef(ridge.mod, s=ridge.lam2)

# prediciton on new data using optimal model (if there is x.test available)
# predict(ridge.cv, s=ridge.lam2, newx=x.test)



### The Lasso
# glmnet with alpha=1 means LASSO
lasso.mod <- glmnet(Salary ~ ., Hitters, alpha=1, use.model.frame=TRUE)
plot(lasso.mod, xvar="lambda", label=TRUE)


# CV for optimal lambda
set.seed(1)
lasso.cv <- cv.glmnet(Salary ~ ., Hitters, alpha=1, use.model.frame=TRUE)
plot(lasso.cv)

# optimal lambda
lasso.lam <- lasso.cv$lambda.min
log(lasso.lam)
points(log(lasso.lam), min(lasso.cv$cvm), cex=3)
#### alternatively, set optimal lambda to lambda.1se for a more parsimonious model ####
lasso.lam2 <- lasso.cv$lambda.1se
log(lasso.lam2)
min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)]
points(log(lasso.lam2), min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)], cex=3)

# plot optimal lambda
plot(lasso.cv$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(lasso.lam), lty=2)
abline(v=log(lasso.lam2), lty=2)


# final model
coef(lasso.cv, s=lasso.lam2)

# prediciton using optimal lambda
# predict(lasso.cv, s=lasso.lam2, newx=x.test, exact=TRUE)



### The Elastic Net with a given alpha
# when 0 < alpha < 1, it becomes elastic net
en.mod <- glmnet(Salary ~ ., Hitters, alpha=0.5, use.model.frame=TRUE)
plot(en.mod, xvar="lambda", label=TRUE)

# CV
set.seed(1)
en.cv <- cv.glmnet(Salary ~ ., Hitters, alpha=0.5, use.model.frame=TRUE)
plot(en.cv)

# optimal lambda
en.lam <- en.cv$lambda.min
log(en.lam)
points(log(en.lam), min(en.cv$cvm), cex=3)
#### alternatively, set optimal lambda to lambda.1se for a more parsimonious model ####
en.lam2 <- en.cv$lambda.1se
log(en.lam2)
min(en.cv$cvm) + en.cv$cvsd[which.min(en.cv$cvm)]
points(log(en.lam2), min(en.cv$cvm) + en.cv$cvsd[which.min(en.cv$cvm)], cex=3)

# plot optimal lambda
plot(en.cv$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(en.lam), lty=2)
abline(v=log(en.lam2), lty=2)


# final model
coef(en.cv, s=en.lam2)

# prediciton using optimal lambda
# predict(en.cv, s=en.lam2, newx=x.test, exact=TRUE)



### The Elastic Net with optimal alpha
set.seed(1)
# CV
en.cva <- cva.glmnet(Salary ~ ., Hitters, use.model.frame=TRUE)
plot(en.cva)

# choose optimal alpha manually
minlossplot(en.cva, cv.type="min")

# final model
plot(en.cva$modlist[[4]])

alpha_opt <- en.cva$alpha[4]
lambda_opt <- en.cva$modlist[[4]]$lambda.min

# plot optimal lambda
plot(en.cva$modlist[[4]]$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(lambda_opt), lty=2)

# coefficients
coef(en.cva, alpha=alpha_opt, s=lambda_opt)



## extract raw data
str(en.cva)
en.cva$alpha
en.cva$modlist

library("plyr")
cva_extract <- function(mod) {
    out <- c()
    out['lambda.min'] <- mod$lambda.min
    out['lambda.1se'] <- mod$lambda.1se
    out['cvm'] <- min(mod$cvm)
    out
}
en.cva.result <- ldply(en.cva$modlist, cva_extract)
en.cva.result$alpha <- en.cva$alpha
en.cva.result




## compare with the regsubset() result last time
#> coef(regfit.best, p_opt)
#(Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI       CWalks    DivisionW      PutOuts      Assists 
#162.5354420   -2.1686501    6.9180175    5.7732246   -0.1300798    1.4082490    0.7743122   -0.8308264 -112.3800575    0.2973726    0.2831680 

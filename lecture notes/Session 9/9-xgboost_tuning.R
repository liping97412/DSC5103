### TUNING xgboost

## input required
# data: dtrain in xgb.DMatrix format
objective <- "reg:linear"
#objective <- "binary:logistic"
cv.fold <- 10

# parameter ranges
max_depths <- c( 1, 2, 4,  6, 8)  # candidates for d 
etas <- c(0.01, 0.005, 0.001, 0.0005, 0.0001)  # candidates for lambda 0.05, 
subsamples <- c(0.5, 0.75, 1) #
colsamples <- c(0.6, 0.8, 1) #


# generate data partition
set.seed(54321)
fold <- sample(rep(seq(cv.fold), length=nrow(dtrain)))
fold.list <- list()
for (k in 1:cv.fold) {
    fold.list[[k]] <- which(fold==k)
}

tune.out <- data.frame()
for (max_depth in max_depths) {
    for (eta in etas) {
        for (subsample in subsamples) {
            for (colsample in colsamples) {
                # **calculate max n.trees by my secret formula**
                n.max <- round(100 / (eta * sqrt(max_depth)))
                if (objective == "reg:linear") {
                    xgb.cv.fit <- xgb.cv(data = dtrain, objective=objective, verbose=0,
                                         folds=fold.list, early_stopping_rounds=100,
                                         nrounds=n.max, max_depth=max_depth, eta=eta, subsample=subsample, colsample_bytree=colsample)
                    n.best <- xgb.cv.fit$best_ntreelimit
                    cv.err <- xgb.cv.fit$evaluation_log$test_rmse_mean[n.best]
                } else if (objective == "binary:logistic") {
                    xgb.cv.fit <- xgb.cv(data = dtrain, objective=objective,  verbose=0,
                                         folds=fold.list, early_stopping_rounds=1000, eval_metric="logloss", # "error", "auc"
                                         nrounds=n.max, max_depth=max_depth, eta=eta, subsample=subsample, colsample_bytree=colsample)
                    n.best <- xgb.cv.fit$best_ntreelimit
                    #cv.err <- xgb.cv.fit$evaluation_log$test_error_mean[n.best]
                    cv.err <- xgb.cv.fit$evaluation_log$test_logloss_mean[n.best]
                    #cv.err <- xgb.cv.fit$evaluation_log$test_auc_mean[n.best]
                }
                out <- data.frame(max_depth=max_depth, eta=eta, subsample=subsample, colsample=colsample, n.max=n.max, nrounds=n.best, cv.err=cv.err)
                print(out)
                tune.out <- rbind(tune.out, out)
            }
        }
    }
}

plot(xgb.cv.fit$evaluation_log$iter, xgb.cv.fit$evaluation_log$test_logloss_mean, type="l")


tune.out

opt <- which.min(tune.out$cv.err)
max_depth.opt <- tune.out$max_depth[opt]
eta.opt <- tune.out$eta[opt]
subsample.opt <- tune.out$subsample[opt]
colsample.opt <- tune.out$colsample[opt]
nrounds.opt <- tune.out$nrounds[opt]

##########################################################
# DSC5103 Statistics
# Session 7. Trees I
# 2018.10
#
# run decision tree using the tree() and cv.tree() function in the "tree" package
# demo for Classification trees
# using the Heart dataset in the "ISLR" book
##########################################################

# data is from http://www-bcf.usc.edu/~gareth/ISL/Heart.csv
heart <- read.csv(file="Heart.csv", row.names=1)

# clean the NA's
heart <- na.omit(heart)
# convert to factors
heart$Sex <- as.factor(heart$Sex)
heart$Fbs <- as.factor(heart$Fbs)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Ca <- as.factor(heart$Ca)
summary(heart)

# split training and test data 50/50
N <- nrow(heart)
set.seed(456)
train.index <- sample(1:N, round(N/2))
test.index <- - train.index



### Decision Tree Models
library("tree")

## grow a tree
tree1 <- tree(AHD ~ ., data=heart, subset=train.index)
tree1
summary(tree1)
# plot the fitted tree
plot(tree1)
text(tree1, cex=0.8)  # denote categorical x with level indice

plot(tree1)
text(tree1, pretty=0, cex=0.8)   # denote categorical x with original level names

plot(tree1)
text(tree1, pretty=TRUE, cex=0.8)   # denote categorical x with simplified level names


## pruning by cross-validation
set.seed(123)
tree1.cv <- cv.tree(tree1, method="misclass")
#tree1.cv <- cv.tree(tree1)
tree1.cv
plot(tree1.cv)

# optimal tree size obtained by CV
optimal <- which.min(tree1.cv$dev)
optimal.k <- tree1.cv$k[optimal]
optimal.size <- tree1.cv$size[optimal]

# the final pruned tree
tree1.pruned <- prune.tree(tree1, best=optimal.size, method="misclass")
#tree1.pruned <- prune.tree(tree1, best=optimal.size)
tree1.pruned
plot(tree1.pruned)
text(tree1.pruned, pretty=TRUE)


## prediction on test data
?predict.tree
tree1.pred <- predict(tree1.pruned, newdata=heart[test.index, ], type="vector")  # "vector" gives probabilities
tree1.pred


## misclassification rate
library("ROCR")
tree1.prediction <- prediction(tree1.pred[,2], heart[test.index, "AHD"])
tree1.err <- performance(tree1.prediction, measure = "err")
plot(tree1.err, ylim=c(0.1, 0.5))

# AUC
as.numeric(performance(tree1.prediction, "auc")@y.values)


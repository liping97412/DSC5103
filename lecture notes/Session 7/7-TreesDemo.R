##########################################################
# DSC5103 Statistics
# Session 7. Trees I
# 2018.10
#
# run decision tree using the tree() and cv.tree() function in the "tree" package
# demo of tree's fit of nonlinear pattern
# using simulated data
##########################################################


### simulate data
set.seed(135790)
N <- 100
# static parameters
x.min <- -1
x.max <- 1
sigma <- 0.2
# function for generating data
x <- runif(n=N, min=x.min, max=x.max)
x <- x[order(x)]
f.x <- sin(pi * x)
epsilon <- rnorm(n=N, mean=0, sd=sigma)
y <- f.x + epsilon
data <- data.frame(x=x, y=y)

# plot data
plot(x, y)  # data points
lines(x, f.x, lwd=4)  # the true model




## grow a tree
library("tree")
tree1 <- tree(y ~ x, data=data)
summary(tree1)
# plot the fitted tree
plot(tree1)
text(tree1)
# plot the partition
plot(x, y)
lines(x, f.x, lwd=4)
partition.tree(tree1, lwd=2, col="red", add=T)


## pruning by cross-validation
set.seed(123)
tree1.cv <- cv.tree(tree1)
tree1.cv
plot(tree1.cv)

# optimal tree size obtained by CV
optimal <- which.min(tree1.cv$dev)
optimal.k <- tree1.cv$k[optimal]
optimal.size <- tree1.cv$size[optimal]

# the final pruned tree
tree1.pruned <- prune.tree(tree1, best=optimal.size)
tree1.pruned
plot(tree1.pruned)
text(tree1.pruned)

# plot pruned tree fit
plot(x, y)  # data points
lines(x, f.x, lwd=4)  # the true model
partition.tree(tree1, lwd=2, col="red", add=T)  # the original whole tree
partition.tree(tree1.pruned, lwd=2, col="blue", add=T)  # the cv pruned tree




### demonstration of tree overfitting
?tree.control
tree2 <- tree(y ~ x, data=data, control=tree.control(nobs=100, mincut=50, mindev=0))
summary(tree2)
plot(tree2)
text(tree2)
tree2

tree3 <- tree(y ~ x, data=data, control=tree.control(nobs=100, minsize=25, mindev=0))
summary(tree3)
tree4 <- tree(y ~ x, data=data, control=tree.control(nobs=100, minsize=20, mindev=0))
summary(tree4)
tree5 <- tree(y ~ x, data=data, control=tree.control(nobs=100, minsize=10, mindev=0))
summary(tree5)
plot(tree5)
tree5

tree6 <- tree(y ~ x, data=data, control=tree.control(nobs=100, minsize=2, mindev=0))
summary(tree6)

# plot overfitted trees
plot(x, y)
lines(x, f.x, lwd=4)
partition.tree(tree2, lwd=2, col="grey50", add=T)
partition.tree(tree3, lwd=2, col="grey40", add=T)
partition.tree(tree4, lwd=2, col="grey30", add=T)
partition.tree(tree5, lwd=2, col="grey20", add=T)
partition.tree(tree6, lwd=2, col="grey10", add=T)


##########################################################
# DSC5103 Statistics
# Session 7. Trees I
# 2018.10
#
# run decision tree using the tree() and cv.tree() function in the "tree" package
# demo for Regression trees
# using the Hitters dataset in package "ISLR"
##########################################################

## get the dataset ready
library("ISLR")

# the Salary column has NA's
sum(is.na(Hitters$Salary))
# remove NA's in Salary
Hitters <- na.omit(Hitters)
sum(is.na(Hitters))
dim(Hitters)
summary(Hitters)

K <- 10
n <- nrow(Hitters)

# plot Salary ~ Years + Hits
library("ggplot2")
p1 <- ggplot(data=Hitters) + geom_point(aes(x=Years, y=Hits, color=log(Salary)), size=5)  + scale_colour_gradientn(colours = rainbow(7)) + theme_bw()
p1



### Decision Tree Models
library("tree")

## grow a tree
?tree
tree1 <- tree(log(Salary) ~ Years + Hits, data=Hitters)
# inspect the tree object
tree1
summary(tree1)
str(tree1)  
tree1$frame  # tree structure data
tree1$where  # assignment of data points to leaves
table(tree1$where)
# plot the fitted tree
plot(tree1)
text(tree1, cex=0.8)  # reduce font size
# plot the partition (only works for 1- or 2-dimensional model)
partition.tree(tree1)
# or overlay on the data points
plot(Hitters$Years, Hitters$Hits, col=grey(9:2/9)[log(Hitters$Salary)*2-6], pch=20, cex=2)
partition.tree(tree1, add=TRUE)


## customize the tree size/depth
?tree.control
tree2 <- tree(log(Salary) ~ Years + Hits, data=Hitters, control=tree.control(nobs=263, minsize=2, mindev=0))
# plot the fitted tree
plot(tree2)
text(tree2, cex=0.8)


## prune the tree
## (this is in-sample!!! the idea is equivalent to regsubsets(method="backward") in linear regression)
?prune.tree
pruned.trees <- prune.tree(tree1)
pruned.trees
plot(pruned.trees)


## pruning by cross-validation
?cv.tree
# use default CV
set.seed(123)
tree1.cv <- cv.tree(tree1)
tree1.cv
plot(tree1.cv)

# use manual balanced partition (cv.tree partitions data in the unbalanced way)
set.seed(123)
fold <- sample(rep(seq(K), length=n))
table(fold)
tree2.cv <- cv.tree(tree1, rand=fold)
tree2.cv
plot(tree2.cv)

# plot the cv error together with the in-sample error
plot(tree1.cv$size, tree1.cv$dev/263, ylim=c(0, 0.9), type="b", col="blue", xlab="Tree Size", ylab="MSE")  # plot the cv error
lines(pruned.trees$size, pruned.trees$dev/263, type="b", col="red")  # add the in-sample error obtained in the previous section
legend(6, 0.8, c("training error", "cv error"), col=c("red", "blue"), lty=c(1, 1), pch=c(21, 21))

# optimal tree size obtained by CV
optimal <- which.min(tree1.cv$dev)
optimal.size <- tree1.cv$size[optimal]

# final pruned tree (using best and using k are equivalent)
tree1.pruned <- prune.tree(tree1, best=optimal.size)
tree1.pruned
plot(tree1.pruned)
text(tree1.pruned)



# plot the segmentation manually
tree1.plot <- p1 + geom_vline(xintercept = 4.5) + geom_segment(aes(x=4.5, y=117.5, xend=25, yend=117.5))  + geom_vline(xintercept = 3.5)
tree1.plot

# or use partition.tree()
plot(Hitters$Years, Hitters$Hits, col=grey(9:2/9)[log(Hitters$Salary)*2-6], pch=20, cex=2)
partition.tree(tree1.pruned, add=TRUE)


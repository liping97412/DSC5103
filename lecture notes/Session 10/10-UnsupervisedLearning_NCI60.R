##########################################################
# DSC5103 Statistics
# Session 10. Unsupervised Learning
# 2018.10
#
# Demo of K-means, GMM, hierarchical clustering, and PCA
# using ISLR Chapter 10 Lab 3: NCI60 Data Example
##########################################################


### The NCI60 data
library("ISLR")
?NCI60
str(NCI60)
x <- NCI60$data
y <- NCI60$labs
dim(x)
table(y)



### PCA on the NCI60 Data
x.pca <- prcomp(x, scale=TRUE)
summary(x.pca)

## visualization
# proportion of variance explained
plot(x.pca)

# PVE
plot(summary(x.pca)$importance[2,],  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(summary(x.pca)$importance[3,], type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")


# visualization using two PC's
Cols <- function(vec){
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
plot(x.pca$x[, c(1, 2)], col=Cols(y), pch=19, xlab="Z1", ylab="Z2")
plot(x.pca$x[, c(1, 3)], col=Cols(y), pch=19, xlab="Z1", ylab="Z3")



### Clustering the Observations of the NCI60 Data
# scale data
sd.data <- scale(x)

## k-means
set.seed(2)
km.out <- kmeans(sd.data, centers=4, nstart=20)
km.clusters <- km.out$cluster
table(km.clusters, y)


## hierarchical clustering
# use Euclidian distance
data.dist <- dist(sd.data)

# use "Complete" linkage
hc.out <- hclust(data.dist)

# keep 4 clusters
hc.clusters <- cutree(hc.out, 4)

# compare with true labels
table(hc.clusters, y)

# visualize clusters with true labels
plot(hc.out, labels=y, cex=0.8)
#abline(h=139, col="red")
rect.hclust(hc.out, k=4, border="red")


## hclust on PCA
hc2.out <- hclust(dist(x.pca$x[,1:5]))  # use 5 PC's
plot(hc2.out, labels=y, main="Hier. Clust. on First Five Score Vectors", cex=0.8)

hc2.clusters <- cutree(hc2.out, 4)
rect.hclust(hc2.out, k=4, border="red")


table(hc2.clusters, hc.clusters)
table(hc2.clusters, y)

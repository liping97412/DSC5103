##########################################################
# DSC5103 Statistics
# Session 10. Unsupervised Learning
# 2018.10
#
# Demo of K-means, GMM, hierarchical clustering, and PCA
# using the Iris dataset in package "ggplot2"
##########################################################


### prepare the data
# load the iris dataset in the ggplot2 package
library("ggplot2")
library("plotly")
?iris
summary(iris)

# preview on Sepal.Length and Sepal.Width
p0 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=Species), size=3) + theme_bw()
p0

plot_ly(data=iris, x=~Sepal.Length, y=~Sepal.Width, z=~Petal.Length, color=~Species)
plot_ly(data=iris, x=~Petal.Length, y=~Sepal.Width, z=~Petal.Width, color=~Species)

# split the data
x <- iris[, 1:4]
y <- iris$Species


### k-means
?kmeans

# single run of K-means leads to sub-optimal result
kmeans(x=x, centers=3)
kmeans(x=x, centers=3)

# multiple runs
set.seed(333)
km <- kmeans(x=x, centers=3, nstart=50)#repeat 50 times and choose the best one
km
km$totss  # total sum of squares
km$betweenss  # between-cluster sum of squares
km$tot.withinss  # sum of withinss
km$withinss  # sum of squares within the clusters
km$centers  # cluster centers
km$cluster  # assignment of points to clusters

# plot the K-means cluster result
iris$km.cluster <- km$cluster
iris$true.cluster <- as.integer(iris$Species)

# 2-D plot the clusters and centroids
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=factor(km$cluster)), size=4) + theme_bw() + geom_point(data=as.data.frame(km$centers), size=6)

plot_ly() %>% add_trace(data=iris, x=~Petal.Length, y=~Sepal.Width, z=~Petal.Width, color=~factor(km$cluster)) %>% add_trace(data=as.data.frame(km$centers),x=~Petal.Length, y=~Sepal.Width, z=~Petal.Width, colors="red")


# plot the predicted cluster together with the true cluster
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=factor(km.cluster)), size=4) + geom_point(aes(color=factor(true.cluster)), size=2) + theme_bw() 

plot_ly() %>% add_trace(data=iris, x=~Petal.Length, y=~Sepal.Width, z=~Petal.Width, color=~factor(true.cluster)) %>% add_trace(data=as.data.frame(km$centers),x=~Petal.Length, y=~Sepal.Width, z=~Petal.Width, colors="red")


# table comparison
table(iris$true.cluster, iris$km.cluster)

# fix the label switching problem
iris$km.cluster <- as.factor(iris$km.cluster)
levels(iris$km.cluster) <- c("2", "1", "3")
# plot again the predicted cluster together with the true cluster
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=factor(km.cluster)), size=4) + geom_point(aes(color=factor(true.cluster)), size=2) + theme_bw() 


## try other K
set.seed(333)
km2 <- kmeans(x=x, centers=2, nstart=50)
km2
km2$tot.withinss
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=factor(km2$cluster)), size=4) + theme_bw()


set.seed(333)
km5 <- kmeans(x=x, centers=5, nstart=50)
km5
km5$tot.withinss
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color=factor(km5$cluster)), size=4) + theme_bw()



### GMM
library("mclust")
?Mclust

## fit a model with default
mc <- Mclust(data=x)
mc
summary(mc)

plot(mc, what="BIC")
plot(mc, what="classification")
plot(mc, what="uncertainty")
plot(mc, what="density")


## fit with 3 clusters
mc3 <- Mclust(data=x, G=3)
summary(mc3)

plot(mc3, what="classification", dimens=c(1, 2))
plot(mc3, what="classification", dimens=c(3, 4))
plot(mc3, what="uncertainty", dimens=c(1, 2))
plot(mc3, what="density")

# comparison with true clusters and K-means
table(y, mc3$classification)
table(km$cluster, mc3$classification)



### hierarchical clustering
?hclust

## build dendogram
# first define distance measure
?dist
d <- dist(x, method = "euclidean")
str(d)

# try different linkages
hc.c <- hclust(d)  # default method="complete"
hc.a <- hclust(d, method="average") 
hc.s <- hclust(d, method="single") 

# plot dendogram
plot(hc.c)
plot(hc.a)
plot(hc.s)


## choose the number of clusters and cut tree
?cutree
hc.c.clusters <- cutree(hc.c, 3)
hc.c.clusters
table(y, hc.c.clusters)

# visualize the cut
plot(hc.c)
abline(h=3.7, lty=2)
# draw dendogram with red borders 
rect.hclust(hc.c, k=3, border="red")


## comparison with true clusters and K-means
table(y, hc.c.clusters)
table(km$cluster, hc.c.clusters)



### PCA
?prcomp

## run PCA on iris data
x.pca <- prcomp(x, scale=TRUE)
x.pca
summary(x.pca)
x.pca$rotation  # loadings of each PC

plot(x.pca)

# proportion of variance explained
plot(summary(x.pca)$importance[2,], type="b", ylim=c(0,1))


# cumulative proportion of variance explained
plot(summary(x.pca)$importance[3,], type="b", ylim=c(0,1))


## plot K-means clustering on PC1 and PC2
x.pca$x
ggplot(data=as.data.frame(x.pca$x), aes(x=PC1, y=PC2)) + geom_point(aes(color=factor(mc3$classification)), size=4) + theme_bw()

plot_ly() %>% add_trace(data=as.data.frame(x.pca$x), x=~PC1, y=~PC2, z=~PC3, color=~factor(mc3$classification)) 


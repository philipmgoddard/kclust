# kclust

Simple k-means clustering. Usage is:

```R
data(iris)

# cluster based on petal length and petal width
clust <- kMeans(iris[, 3:4], 3)
```

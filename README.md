# k-means clustering

Simple k-means clustering. Usage is:

```R
data(iris)

# cluster based on petal length and petal width
clust <- kMeans(iris[, 3:4], nCentroids = 3)
```

I havent bothered making this into a package, as there is a fine kmeans() function in the stats package which is no doubt waaaay better than this. This is for education purposes only!

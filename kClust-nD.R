# k means clustering - generalises to n dimensions
# with arbitrary integer (>0) number of clusters
# assumes positive space only

restrictRange <- function(x) {
  return(x / max(x))
}

initCentroid <- function(nCent, df) {
  return(lapply(seq(nCent), function(x) as.numeric(df[sample(nrow(df), 1), ])))
}

euclDist <- function(x, y) {
  return(sqrt(sum(x - y) ^ 2))
}

COM <- function(x) {
  return(sum(x) / length(x))
}

kMeans <- function(df, nCentroids = 1, verboseIter = FALSE) {
  # process input
  nDim <- ncol(df)
  scaleInput <- df
  scaleInput[] <- lapply(scaleInput, restrictRange)
  centroids <- initCentroid(nCentroids, scaleInput)
  scaleInput$grp <- NA
  inputMat <- data.matrix(scaleInput)

  count <- 0
  repeat {
    count <- count + 1
    if (verboseIter) cat(paste0("iteration ", count, sep = ""), "\n")

    # assign each point to a group based on closest centroid
    inputMat[, nDim + 1] <- apply(inputMat[, 1:nDim], 1, function(x) {
      tmp <- lapply(seq(nCentroids), function(y) euclDist(x, centroids[[y]]))
      which.min(tmp)
    })

    # update centroids
    oldCentroids <- centroids
    centroids <- lapply(seq(nCentroids), function(x) {
      tmp <- inputMat[inputMat[, ncol(inputMat)] == x, 1:(ncol(inputMat) - 1)]
      apply(tmp, 2, COM)
    })

    # check for convergence
    conv <- sum(abs(unlist(Map(function(x, y) x - y, centroids, oldCentroids))))
    if (conv < 1e-6) break
  }

  # scale back centroids to origional scale
  centroids <- lapply(centroids, function(x) {
    unlist(lapply(seq(nDim), function(y) x[[y]] * max(df[, y])))
  })

  # prepare and return output
  df$grp <- inputMat[, ncol(inputMat)]
  out <- list(data = df,
              centroids = centroids,
              iterations = count)
  return(out)
}

#################################################
#################################################
# test
library(ggplot2)
library(gtable)
library(gridExtra)
data(iris)

# cluster based on petal length and petal width (can do more if like!)
clust <- kMeans(iris[, 3:4], 3)

# note that you probably wont have my color scheme loaded
plot1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  theme_bw() +
  scale_color_manual(values = philTheme()) +
  theme(legend.position = c(0.1, 0.7))

plot2 <- ggplot(clust$data, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = factor(grp)), size = 3, alpha = 0.6) +
  theme_bw() +
  scale_color_manual(values = philTheme(), name = "Group") +
  theme(legend.position = c(0.08, 0.7))

gA <- ggplotGrob(plot1)
gB <- ggplotGrob(plot2)

grid.newpage()
grid.draw(rbind(gA, gB, size = "first"))

# scale up - sample with replacement and add a bit of noise
samples <- sample(nrow(iris), 100000, replace = TRUE)
data2 <- iris[samples, ]
set.seed(1234)
data2[, 1:4] <- lapply(data2[, 1:4], function(x) {
  x + rnorm(n = 10000, mean = 0, sd = 0.1))
}

# does the job. screams out to be written in compiled language though...
clust2 <- kMeans(data2[, 3:4], 3, verboseIter = TRUE)

plot3 <- ggplot(clust2$data, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = factor(grp)), alpha = 0.6) +
  theme_bw() +
  scale_color_manual(values = philTheme(), name = "Group") +
  theme(legend.position = c(0.1, 0.8))

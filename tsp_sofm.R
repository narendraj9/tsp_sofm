#!/usr/bin/env Rscript
# (Class Assignment | Narendra Joshi)
# (Travelling Salesman Problem Using an SOFM)

library(calibrate)

# intial values of parameters
alpha <- 0.89
epochs <- 100
  
# load info about cities
# input csv file has three columns, the last of which
# are coordinates
cities <- read.csv("cities.csv", header=TRUE)
ncities <- length(cities[,1])

# initial neighbourhood order =
# all the neurons
norder <- floor(ncities/2)

# coordinates of the cities
X <- as.matrix(cities[,2:3])

# initial weights of the circular feature map
w <- cbind(runif(ncities, min(X[,1]), max(X[,1])),
           runif(ncities, min(X[,2]), max(X[,2])))

# function for finding norm of a vector
norm <- function(x) {
  return(sqrt(sum(x^2)))
}

# get the value of alpha for a given node
getAlpha <- function(i, iwnode) {
  if (norder) {
    d <- abs(i - iwnode)
    dist <- min(d, abs(ncities - d))
    return(alpha * exp(-(((dist)^2)/norder)))
  }
  else if (i == iwnode)
    return(alpha)
  else return(0)
}

# update weights; iwnode = winning node index
getNewWeights <- function(w, x, alpha, norder) {
 # get the winner node index
  xrep <- matrix(rep(x, ncities), nrow = ncities, byrow=T)
  diff <- (xrep - w)
  # winner node index
  iwnode <- which.min(apply(diff, 1, norm))
  
 # alpha-matrix for different nodes in neighbourhood
  for (i in 1:ncities) {
    if (i == 1) {
      malpha <- rep(getAlpha(i, iwnode), 2)
    }
    else {
      malpha <- rbind(malpha, rep(getAlpha(i, iwnode), 2))
    }
  }
  w <- w + (malpha * diff)
  return(w)
}

plot(X, pch=20)
points(w, pch="*", col="red")
# training period
while (norder >= 0) {
  for (t in 1:epochs) {
      for (i in 1:ncities) {
        cat ("Epoch #", t," presenting city #", i, "\n")
        w <- getNewWeights(w, X[i,], alpha, norder)
      }
    }
  norder <- (norder - 1)
  alpha <- (alpha - alpha/10)
                                        # plot weights and points
  plot(X, pch=20, cex=0.8)
  points(w, col="red", pch="*")
}
cityOrder <- rep(0,ncities)

# reading results and plotting the results
plot(X, xlab="x", ylab="y", pch='*')
for (i in 1:ncities) {
  xrep <- matrix(rep(X[i,], ncities), nrow = ncities, byrow = T)
  diff <- (xrep - w)
  # winner node index
  cityOrder[i] <- which.min(apply(diff, 1, norm))
}

# lets label cities in order
textxy(X[,1], X[,2], cityOrder, offset = 1, cex=0.8)
dev.copy(jpeg, "city-plot.jpg")
dev.off()

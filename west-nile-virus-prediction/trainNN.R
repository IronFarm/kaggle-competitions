source("loadData.R")

sigmoid <- function(z) {
  1/(1 + exp(-z))
}

sigmoidGradient <- function(z) {
  sigmoid(z) * (1 - sigmoid(z))
}

getCost <- function(y, hyp) {
  m <- nrow(y)
  
  J <- sum(-y * log(hyp) - (1 - y) * log(1 - hyp))/m
}

nIter <- 200
nThresholds <- 100
lambda <- 0.1
m <- nrow(train)

# X variables are bias node, day of year, lat, long and num of mosquitos
X <- cbind(rep(1, m),
           train$DayOfYear,
           train$Latitude,
           train$Longitude,
           train$NumMosquitos)

# y contains 0 for no WNV and 1 for WNV present
y <- matrix(as.integer(train$WnvPresent))

# Randomly initialise weights
theta1 <- matrix(rnorm(20), nrow = 4, ncol = 5)
theta2 <- matrix(rnorm(5), nrow = 1, ncol = 5)

# Gradient descent
costHist <- matrix(nrow = nIter, ncol = 2)
for (i in 1:nIter) {
  # Calculate hidden layer
  z2 <- X %*% t(theta1)
  a2 <- cbind(rep(1, m),
              sigmoid(z2))
  
  # Calculate output layer
  z3 <- a2 %*% t(theta2)
  hyp <- sigmoid(z3)
  
  J <- getCost(y, hyp)
  costHist[i, ] <- c(i, J)
  
  delta3 <- hyp - y
  delta2 <- (delta3 %*% theta2)[, 2:5] * sigmoidGradient(X %*% t(theta1))
  
  grad2 <- (t(delta3) %*% a2)/m
  grad1 <- (t(delta2) %*% X)/m
  
  theta1 <- theta1 - grad1 * lambda
  theta2 <- theta2 - grad2 * lambda
}

# Find threshold which maximises F1 score (shouldn't be done on training data though!)
f1Hist <- matrix(nrow = nThresholds, ncol = 2)
for (i in 1:nThresholds) {
  threshold <- i/nThresholds
  
  decision <- hyp > threshold
  
  tp <- sum(decision == 1 & y == 1)
  fp <- sum(decision == 1 & y == 0)
  fn <- sum(decision == 0 & y == 1)
  tn <- sum(decision == 0 & y == 0)
  
  precision <- tp/(tp + fp)
  recall <- tp/(tp + fn)
  
  f1 <- 2 * precision * recall/(precision + recall)
  f1Hist[i, ] <- c(i, f1)
}
